# slack-go
Play go over slack
> If you're on slack, you're technically working.

A hack to play the game [go (AKA weiqi, baduk)](https://en.wikipedia.org/wiki/Go_(game)) with slack slash commands. The server will respond to moves by generating and image, uploading it to imgur, and posting a link to the image in the channel/DM.

![playing a move](http://i.imgur.com/kI4cWcO.jpg)

## Setup
Just add a [slash command](https://my.slack.com/services/new/slash-commands) to your team (a command of go is assumed for this readme), point it wherever you deploy this server.
### Heroku
You can run the server on heroku - you can get help with this [here](https://devcenter.heroku.com/articles/getting-started-with-clojure#set-up). Basically, install the heroku toolbelt, `heroku login`, add a remote `heroku git:remote -a <app-name>`, and push.

#### free plan warning 
If you use a free heroku instance, your app will spin down after an hour with no requests. This will cause you two annoying problems.

1. Game state is only stored in memory, so it doesn't persist across app restarts. Sorry.
2. It takes a really long time for the app to spin up after being idle, so you'll invariably get told your slack command timed out on your first attempt to use the app after going idle. Just wait a few seconds and try again.

## Supported Commands
```/go start <user1> <user2> [board-dimension]```

Start a game with `user1` playing black and `user2` playing white. `board-dimension` defaults to 9.

```/go play <move>```

Plays a move. Moves look like `d6`

```/go show```

Image of the current board state

```/go pass```

Allow the other player to go

```/go score [komi]```

Score a finished game with a naive algorithm

```/go end```

Finish the game, which allows another game to start on the channel or DM

```/go kick```

Wake up the bot (useful for an idle heroku instance)

```/go help```

Display this help text
