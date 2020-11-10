import fetch from "node-fetch";
import qs from "qs";

export default (req, res) => {
  const query = qs.stringify(req.query, {
    arrayFormat: "repeat",
    skipNulls: true,
    addQueryPrefix: false,
  });

  fetch(`https://api.twitch.tv/helix/streams?${query}`, {
    headers: {
      Authorization: `Bearer ${process.env.TWITCH_TOKEN}`,
      "Client-Id": process.env.TWITCH_CLIENT_ID || "",
    },
  })
    .then((res) => Promise.all([res.status, res.json()]))
    .then(([status, body]) => res.status(status).json(body));
};
