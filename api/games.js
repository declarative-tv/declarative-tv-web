import fetch from "node-fetch";
import qs from "qs";

const allowCors = (fn) => (req, res) => {
  res.setHeader("Access-Control-Allow-Credentials", true);
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader(
    "Access-Control-Allow-Methods",
    "GET,OPTIONS,PATCH,DELETE,POST,PUT"
  );
  res.setHeader(
    "Access-Control-Allow-Headers",
    "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version"
  );

  if (req.method === "OPTIONS") {
    res.status(200).end();
  } else {
    fn(req, res);
  }
};

export default allowCors((req, res) => {
  const query = qs.stringify(req.query, {
    arrayFormat: "repeat",
    skipNulls: true,
    addQueryPrefix: false,
  });

  fetch(`https://api.twitch.tv/helix/games?${query}`, {
    headers: {
      Authorization: `Bearer ${process.env.TWITCH_TOKEN}`,
      "Client-Id": process.env.TWITCH_CLIENT_ID || "",
    },
  })
    .then((res) => Promise.all([res.status, res.json()]))
    .then(([status, body]) => res.status(status).json(body));
});
