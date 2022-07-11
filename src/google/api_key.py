import random
from collections import defaultdict

from src.utils.logging import logger


class ApiKeyException(Exception):
    pass


class ApiKey:
    def __init__(self, all_keys):
        self.valid_keys: list[str] = all_keys
        random.shuffle(self.valid_keys)
        self.invalid_keys: set(str) = set()
        self.current_keys: dict[str] = defaultdict(str)
        self.key_usage: dict[int] = defaultdict(int)

    def use(self, units: int):
        cur_key = self.current_keys[units]
        if not cur_key:
            cur_key = self.valid_keys.pop()
            self.current_keys[units] = cur_key
        self.key_usage[units] += 1
        return cur_key

    def switch_key(self, units: int):

        self.invalid_keys.add(self.current_keys[units])

        try:
            new_key = self.valid_keys.pop()
            logger.warning(
                "Switching key (units=%d) after %d uses. %d keys left",
                units,
                self.key_usage[units],
                len(self.valid_keys),
            )
            self.key_usage[units] = 0
        except KeyError:
            raise ApiKeyException("No valid API keys left!")

        self.current_keys[units] = new_key

    def __len__(self):
        return len(self.valid_keys)
