import fetch from "node-fetch";
import * as qs from "qs";

export default (req, res) => {
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
