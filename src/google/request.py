import json

import requests
from requests.exceptions import HTTPError

from src import API_KEYS
from src.google.api_key import ApiKey


class YtRequestorError(Exception):
    pass


class YtRequestor:
    def __init__(self) -> None:

        self.base_url = "https://www.googleapis.com/youtube/v3/search"
        self.keys = ApiKey(API_KEYS)

    def _recommended_videos(self, video_id: str, key: str):
        params = {
            "part": "snippet",
            "relatedToVideoId": video_id,
            "type": "video",
            "maxResults": "50",
            "key": key,
        }
        resp = requests.get(self.base_url, params=params)
        response = json.loads(resp.text)

        if resp.status_code == 200:
            return response
        elif resp.status_code == 400:
            if response["error"]["message"] == "API key not valid. Please pass a valid API key.":
                raise YtRequestorError("invalid api key: {}".format(key))
            elif response["error"]["message"] == "Request contains an invalid argument.":
                raise HTTPError("Could not find video id: {}".format(video_id))

        raise HTTPError("resp code: {}\n{}".format(resp.status_code, response))

    def _search(self, query: str, key: str):
        params = {
            "type": "video",
            "part": "snippet",
            "maxResults": "5000",
            "q": query,
            "key": key,
        }

        resp = requests.get(self.base_url, params=params)
        response = json.loads(resp.text)

        if resp.status_code == 200:
            return response
        elif resp.status_code == 400:
            if response["error"]["message"] == "API key not valid. Please pass a valid API key.":
                raise YtRequestorError("invalid api key: {}".format(key))

        raise HTTPError("resp code: {}\n{}".format(resp.status_code, response))

    def search(self, query: str):
        try:
            response = self._search(query, self.keys.use())
        except Exception as e:
            raise e

        return response

    def get_recommended_videos(self, video_id: str):
        try:
            response = self._recommended_videos(video_id, self.keys.use())
        except YtRequestorError:
            self.keys.switch_key()
            try:
                response = self._recommended_videos(video_id, self.keys.use())
            except Exception as e:
                raise e
        return response
