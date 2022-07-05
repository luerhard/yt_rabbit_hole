import asyncio

import aiohttp

from src.utils import setup_logging


logger = setup_logging(streamlevel="INFO", filelevel=None)


class BaseAsyncCrawler:

    HEADER = {
        "User-Agent": (
            "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36"
            " (KHTML, like Gecko) Chrome/80.0.3987.163 Safari/537.36"
        ),
        "Connection": "keep-alive",
        "Accept": "text/html",
        "Accept-Charset": "utf-8",
    }

    TIMEOUT = aiohttp.ClientTimeout(total=60)
    MAX_CONCURRENCY = 30

    def urls(self):
        raise NotImplementedError

    async def download(self, session, url):
        raise NotImplementedError

    @staticmethod
    async def update_tasks(tasks):
        done, _ = await asyncio.wait(tasks, timeout=1)
        tasks.symmetric_difference_update(done)

    async def async_run(self, loop):
        tasks = set()
        async with aiohttp.ClientSession() as session:
            for url in self.urls():
                while len(tasks) >= self.MAX_CONCURRENCY:
                    await self.update_tasks(tasks)
                task = asyncio.ensure_future(self.download(session, url), loop=loop)
                tasks.add(task)
            while tasks:
                await self.update_tasks(tasks)

    def run(self):
        loop = asyncio.new_event_loop()
        loop.run_until_complete(self.async_run(loop))
        loop.close()
