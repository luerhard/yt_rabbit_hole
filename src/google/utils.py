import networkx as nx
import pandas as pd
from tqdm.auto import tqdm


def row_info(row):
    result = {}

    result["video_id"] = row["id"]["videoId"]
    snippet = row.get("snippet", {})
    if isinstance(snippet, float):
        return result

    result["date"] = snippet.get("publishedAt")
    result["title"] = snippet.get("title")
    result["description"] = snippet.get("description")
    result["channelTitle"] = snippet.get("channelTitle")

    return result


def extract_info(df):
    if df.empty:
        return df
    try:
        infos = pd.DataFrame(df.apply(row_info, axis=1).tolist())
    except Exception as e:
        print(df)
        raise e

    return pd.concat([df, infos], axis=1)


def run_step(tube, data, max_results):

    last_step = max(data["step"])
    step = last_step + 1

    # find uncrawled video ids
    crawled_video_ids = set(data.loc[data.step < last_step, "video_id"])

    video_ids = set(data.loc[data.step == last_step, "video_id"])
    video_ids = video_ids.symmetric_difference(crawled_video_ids)

    for video_id in tqdm(video_ids):
        result = tube.get_recommended_videos(video_id, max_results=max_results)
        temp = pd.DataFrame(result)
        temp = extract_info(temp)
        temp["source_video_id"] = video_id
        temp["step"] = step
        temp["search_rank"] = range(1, len(temp) + 1)
        data = pd.concat([data, temp])

    return data


def create_network(data):
    g = nx.DiGraph()

    # add nodes
    for row in data.loc[data.step == 0, :].itertuples():
        g.add_node(
            row.video_id,
            data=row.date,
            title=row.title,
            description=row.description,
            channel_title=row.channelTitle,
            step=row.step,
        )

    # add edges
    for row in data.itertuples():
        u = row.source_video_id
        v = row.video_id
        if row.step != 0 and g.has_node(v):
            g.add_edge(u, v, rank=row.search_rank)

    return g
