import networkx as nx
import numpy as np
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
    result["channel_title"] = snippet.get("channelTitle")
    result["channel_id"] = snippet.get("channelId")

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

        metadata = tube.get_video_metadata(video_id)
        for name, val in metadata.items():
            data.loc[(data.video_id == video_id) & (data.step == 0), name] = val

    return data


def create_network(data):
    g = nx.DiGraph()

    # add nodes
    for row in data.itertuples():
        if not g.has_node(row.video_id):
            g.add_node(
                row.video_id,
                step=row.step,
                date=row.date,
                title=row.title,
                description=row.description,
                channel_title=row.channel_title,
                channel_id=row.channel_id,
                fav_count=int(row.fav_count) if not np.isnan(row.fav_count) else -1,
                view_count=int(row.view_count) if not np.isnan(row.view_count) else -1,
                like_count=int(row.like_count) if not np.isnan(row.like_count) else -1,
                comment_count=int(row.comment_count) if not np.isnan(row.comment_count) else -1,
                duration=row.duration,
            )

    # add edges
    for row in data.itertuples():
        u = row.source_video_id
        v = row.video_id
        if row.step != 0 and g.has_node(v) and not g.has_edge(u, v):
            g.add_edge(u, v, rank=row.search_rank)

    return g
