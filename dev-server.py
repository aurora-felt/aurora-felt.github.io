import io
import aiohttp
from aiohttp import web
import asyncio
from watchfiles import awatch
import subprocess

# change these to whatever
hostName = "localhost"
serverPort = 8080

async def index_handler(_):
    return web.Response(text=io.open("index.html","r").read(),content_type="text/html")

async def index_js_handler(_):
    return web.Response(text=io.open("dev-reload.js","r").read(),content_type="text/javascript")

async def posts_handler(_):
    return web.Response(text=io.open("./posts.json").read(), content_type="application/json")

async def about_handler(_):
    return web.Response(text=io.open("./about.json").read(), content_type="application/json")

async def colors_handler(_):
    return web.Response(text=io.open("./colors.json").read(), content_type="application/json")

async def image_handler(request):
    return web.FileResponse(path="./images/{}".format(request.match_info['name']))

async def links_handler(_):
    return web.Response(text=io.open("./links.json").read(), content_type="application/json")

async def post_handler(request):
    return web.Response(text=io.open("./posts/{}".format(request.match_info['name'])).read(), content_type="text/utf-8")


CLIENTS = set()

async def websocket_handler(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)
    CLIENTS.add(ws)
    async for msg in ws:
        if msg.type == aiohttp.WSMsgType.ERROR:
            print('ws connection closed with exception %s' % ws.exception())
            CLIENTS.remove(ws)
    return ws

async def watch_recompile():
   async for _ in awatch("./src"):
       subprocess.run(["spago","bundle","--outfile","dev-reload.js","--minify"])
       subprocess.run(["spago","bundle","--module","Blog","--minify"])
       await broadcast("reload")

async def watch_content():
   async for _ in awatch("./"):
       await broadcast("reload")


async def broadcast(message):
    for websocket in CLIENTS.copy():
        try:
            await websocket.send_str(message)
        except:
            CLIENTS.remove(websocket)

def create_runner():
    app = web.Application()
    app.add_routes([
        web.get('/',   index_handler),
        web.get('/index.js', index_js_handler),
        web.get('/posts.json', posts_handler),
        web.get('/links.json', links_handler),
        web.get('/about.json', about_handler),
        web.get('/colors.json', colors_handler),
        web.get('/images/{name}', image_handler),
        web.get('/posts/{name}', post_handler),
        web.get('/ws', websocket_handler),
    ])
    return web.AppRunner(app)

async def start_server(host=hostName, port=serverPort):
    print("Server started http://%s:%s" % (hostName, serverPort))
    runner = create_runner()
    await runner.setup()
    site = web.TCPSite(runner, host, port)
    await site.start()

asyncio.set_event_loop(asyncio.new_event_loop())
_ = asyncio.ensure_future(watch_recompile())
_ = asyncio.ensure_future(watch_content())

if __name__ == "__main__":
    loop = asyncio.get_event_loop()
    loop.run_until_complete(start_server())
    loop.run_forever()
