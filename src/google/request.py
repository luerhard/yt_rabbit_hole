import json
from typing import Dict
from typing import Optional
from typing import Union

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

    def _recommended_videos(self, video_id: str, key: str, max_results: int):
        params: Dict[str, Union[int, str]] = {
            "part": "snippet",
            "relatedToVideoId": video_id,
            "type": "video",
            "maxResults": max_results,
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
        elif resp.status_code == 403:
            raise YtRequestorError("exceeded API quota: {}".format(key))
        elif resp.status_code == 404:
            return {
                "id": {"videoId": video_id},
                "items": {},
            }

        raise HTTPError("resp code: {}\n{}".format(resp.status_code, response))

    def get_recommended_videos(self, video_id: str, max_results: int = 50):

        while len(self.keys.valid_keys) >= 0:
            try:
                response = self._recommended_videos(
                    video_id,
                    self.keys.use(),
                    max_results=max_results,
                )
                return response
            except YtRequestorError:
                self.keys.switch_key()
            except Exception as e:
                raise e

    def _search(self, query: str, key: str, max_results: int, page_token: Optional[str] = None):
        params: Dict[str, Union[int, str]] = {
            "type": "video",
            "part": "snippet",
            "maxResults": max_results,
            "q": query,
            "key": key,
        }

        if page_token:
            params["pageToken"] = page_token

        resp = requests.get(self.base_url, params=params)
        response = json.loads(resp.text)

        if resp.status_code == 200:
            return response
        elif resp.status_code == 400:
            if response["error"]["message"] == "API key not valid. Please pass a valid API key.":
                raise YtRequestorError("invalid api key: {}".format(key))
        elif resp.status_code == 403:
            raise YtRequestorError("exceeded API quota: {}".format(key))

        raise HTTPError("resp code: {}\n{}".format(resp.status_code, response))

    def search(self, query: str, max_results: int = 50):

        while len(self.keys.valid_keys) >= 0:
            try:
                response = self._search(
                    query,
                    self.keys.use(),
                    max_results=max_results,
                    page_token=None,
                )
                return response
            except YtRequestorError:
                self.keys.switch_key()
            except Exception as e:
                raise e
