from src.google.request import YtRequestor


def test_single_search():
    tube = YtRequestor()
    results = tube.search("test me", max_results=10)
    assert len(results) == 10


def test_search_pagination():
    tube = YtRequestor()
    results = tube.search("test me hard", max_results=102)
    assert len(results) == 102


def test_recommended_videos_50():
    tube = YtRequestor()
    results = tube.get_recommended_videos("zDztJN9-o4c", max_results=50)
    assert len(results) == 50


def test_recommended_videos_lessthan50():
    tube = YtRequestor()
    results = tube.get_recommended_videos("zDztJN9-o4c", max_results=12)
    assert len(results) == 12


def test_recommended_videos_pagination():
    tube = YtRequestor()
    results = tube.get_recommended_videos("zDztJN9-o4c", max_results=52)
    assert len(results) == 52


def test_video_metadata():
    tube = YtRequestor()
    results = tube.get_video_metadata("zDztJN9-o4c")
    assert len(results) == 1
