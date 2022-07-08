from src.google.request import YtRequestor


def test_single_search():
    tube = YtRequestor()
    results = tube.search("test me", max_results=10)
    assert len(results) == 10


def test_search_pagination():
    tube = YtRequestor()
    results = tube.search("test me hard", max_results=555)
    assert len(results) == 51


def test_recommended_videos():
    tube = YtRequestor()
    results = tube.get_recommended_videos("zDztJN9-o4c", max_results=50)
    assert len(results) == 50


def test_recommended_videos_pagination():
    tube = YtRequestor()
    results = tube.get_recommended_videos("zDztJN9-o4c", max_results=52)
    assert len(results) == 52
