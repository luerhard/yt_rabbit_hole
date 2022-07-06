from src.google.api_key import ApiKey


def test_simple():
    key = ApiKey(["123", "testme", "A_d%2;"])
    assert key.use() == "123"
    key.switch_key()
    assert key.use() == "testme"
