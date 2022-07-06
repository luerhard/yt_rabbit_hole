from src.utils.logging import logger


class ApiKeyException(Exception):
    pass


class ApiKey:
    def __init__(self, all_keys):
        self.valid_keys: list[str] = all_keys[::-1]
        self.invalid_keys: set(str) = set()
        self.current_key: str = self.valid_keys.pop()

    def use(self):
        return self.current_key

    def switch_key(self):
        self.invalid_keys.add(self.current_key)

        try:
            new_key = self.valid_keys.pop()
            logger.warning("Switching key from %s to %s", self.current_key, new_key)
        except KeyError:
            raise ApiKeyException("No valid API keys left!")

        self.current_key = new_key

    def __len__(self):
        return len(self.valid_keys)
