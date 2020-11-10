import { NowRequest, NowResponse } from "@vercel/node";
import fetch from "node-fetch";
import * as qs from "qs";

export default (req: NowRequest, res: NowResponse) => {
  const query = qs.stringify(req.query, {
    arrayFormat: "repeat",
    skipNulls: true,
    addQueryPrefix: false,
  });

  fetch(`https://api.twitch.tv/helix/streams?${query}`, {
    headers: {
      Authorization: `Bearer ${process.env.TOKEN}`,
      "Client-Id": process.env.CLIENT_ID || "",
    },
  })
    .then((res) => Promise.all([res.status, res.json()]))
    .then(([status, body]) => res.status(status).json(body));
};
