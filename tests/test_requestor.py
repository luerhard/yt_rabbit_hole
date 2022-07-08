from src.google.request import YtRequestor


def test_single_search():
    tube = YtRequestor()
    results = tube.search("test me", max_results=10)
    assert len(results) == 10


def test_search_pagination():
    tube = YtRequestor()
    results = tube.search("test me hard", max_results=51)
    assert len(results) == 51
