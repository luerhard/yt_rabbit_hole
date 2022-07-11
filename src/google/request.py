import json
from typing import Dict
from typing import Union
from urllib.parse import urljoin

import numpy as np
import requests
from requests.exceptions import HTTPError

from src import API_KEYS
from src.google.api_key import ApiKey


class YtRequestorError(Exception):
    pass


class YtRequestor:
    def __init__(self) -> None:

        self.base_url = "https://www.googleapis.com/youtube/v3/"
        self.search_url = urljoin(self.base_url, "search")
        self.video_url = urljoin(self.base_url, "videos")
        self.keys = ApiKey(API_KEYS)

    def _single_request(
        self,
        func,
        q: str,
        units: int,
        page_token: str | None = None,
        max_results: int = 50,
    ):
        while len(self.keys.valid_keys) >= 0:
            try:
                response = func(
                    q,
                    self.keys.use(units=units),
                    max_results=max_results,
                    page_token=page_token,
                )
                return response
            except YtRequestorError:
                self.keys.switch_key(units=units)
            except Exception as e:
                raise e

        return response

    def _multi_request(self, func, q: str, units: int, max_results: int = 50):

        n_iter = max_results // 50
        rest = max_results % 50

        page_token = None
        results = []

        for i in range(n_iter):
            response = self._single_request(
                func=func,
                q=q,
                units=units,
                page_token=page_token,
                max_results=max_results,
            )
            results.extend(response["items"])
            page_token = response.get("nextPageToken")
            if not page_token:
                break
        else:
            if rest > 0:
                response = self._single_request(
                    func=func,
                    q=q,
                    units=units,
                    page_token=page_token,
                    max_results=rest,
                )
                results.extend(response["items"])

        return results

    def _recommended_videos(
        self,
        video_id: str,
        key: str,
        max_results: int,
        page_token: str | None = None,
    ):
        params: Dict[str, Union[int, str]] = {
            "part": "snippet",
            "relatedToVideoId": video_id,
            "type": "video",
            "maxResults": max_results,
            "key": key,
            "relevanceLanguage": "en",
            "safeSearch": "none",
            "regionCode": "us",
        }

        if page_token:
            params["pageToken"] = page_token

        resp = requests.get(self.search_url, params=params)
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

    def _search(self, query: str, key: str, max_results: int, page_token: str | None = None):
        params: Dict[str, Union[int, str]] = {
            "type": "video",
            "part": "snippet",
            "maxResults": max_results,
            "q": query,
            "key": key,
            "relevanceLanguage": "en",
            "safeSearch": "none",
            "regionCode": "us",
        }

        if page_token:
            params["pageToken"] = page_token

        resp = requests.get(self.search_url, params=params)
        response = json.loads(resp.text)

        if resp.status_code == 200:
            return response
        elif resp.status_code == 400:
            if response["error"]["message"] == "API key not valid. Please pass a valid API key.":
                raise YtRequestorError("invalid api key: {}".format(key))
        elif resp.status_code == 403:
            raise YtRequestorError("exceeded API quota: {}".format(key))

        raise HTTPError("resp code: {}\n{}".format(resp.status_code, response))

    def _video_metadata(self, video_id: str, key: str, **kwargs):
        params = {
            "part": ["snippet", "contentDetails", "statistics"],
            "id": video_id,
            "key": key,
        }
        resp = requests.get(self.video_url, params=params)
        response = json.loads(resp.text)

        if resp.status_code == 200:
            return response
        elif resp.status_code == 400:
            if response["error"]["message"] == "API key not valid. Please pass a valid API key.":
                raise YtRequestorError("invalid api key: {}".format(key))
        elif resp.status_code == 403:
            raise YtRequestorError("exceeded API quota: {}".format(key))

        raise HTTPError("resp code: {}\n{}".format(resp.status_code, response))

    def _search_video(self, video_id: str, key: str, **kwargs):
        params = {
            "part": ["snippet"],
            "id": video_id,
            "key": key,
        }
        resp = requests.get(self.video_url, params=params)
        response = json.loads(resp.text)

        if resp.status_code == 200:
            return response
        elif resp.status_code == 400:
            if response["error"]["message"] == "API key not valid. Please pass a valid API key.":
                raise YtRequestorError("invalid api key: {}".format(key))
        elif resp.status_code == 403:
            raise YtRequestorError("exceeded API quota: {}".format(key))

        raise HTTPError("resp code: {}\n{}".format(resp.status_code, response))

    def search_video(self, video_id: str):
        response = self._single_request(self._search_video, q=video_id, units=1)
        return response

    def get_video_metadata(self, video_id: str):
        response = self._single_request(self._video_metadata, video_id, units=1)
        item = response["items"][0]
        stats = item["statistics"]
        details = item["contentDetails"]
        snippet = item["snippet"]

        r = dict()
        r["view_count"] = float(stats.get("viewCount", np.nan))
        r["like_count"] = float(stats.get("likeCount", np.nan))
        r["fav_count"] = float(stats.get("favoriteCount", np.nan))
        r["comment_count"] = float(stats.get("commentCount", np.nan))
        r["duration"] = details.get("duration", "")
        r["description"] = snippet.get("description", "")

        return r

    def get_recommended_videos(self, video_id: str, max_results: int = 50):
        response = self._multi_request(
            self._recommended_videos,
            q=video_id,
            units=100,
            max_results=max_results,
        )
        return response

    def search(self, query: str, max_results: int = 50):
        response = self._multi_request(
            self._search,
            q=query,
            units=100,
            max_results=max_results,
        )
        return response
