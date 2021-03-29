client_id="$1"
client_secret="$2"

url="https://id.twitch.tv/oauth2/token?client_id=$client_id&client_secret=$client_secret&grant_type=client_credentials"

vercel secrets rm twitch-token

vercel secrets add twitch-token $(curl -XPOST "$url" | jq '.access_token')
