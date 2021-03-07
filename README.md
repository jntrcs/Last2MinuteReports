# Last2MinuteReports
Web scraper and Twitter Bot Poster for @JazzRefs

This project scrapes all Last 2 Minute reports from the official NBA website. 

For each Jazz game, TwitterBotPoster.R checks to see if a game is scheduled, pulls the details about the referees, updates the two .csvs to include any games that had happened in the last few days, calculates the rankings of each ref, and posts the update to Twitter. 

Our main stat is the number of bad calls per minute reported in the Last Two Minute Reports. A bad call is defined as one identified as an incorrect call or an incorrect non-call. Minutes are 2 minutes per game, 4 if they went to OT, 6 if double OT, etc. Ties in the rankings are broken by whoever participated in more calls defined as "correct calls" (or correct non-calls). If there are still ties, we default to whoever had more minutes reffed.

## Build for another team?

If you're interested in taking this idea and building a bot for your favorite team, feel free! I only ask that you fork the repository (don't just steal the scripts) and give credit in your own repository. To customize you'll need to update team_prefix in TwitterBotPoster.R, set up a Twitter account with API credentials where you want it to post, and possibly a few other ad hoc things. If you want to try to set it up and get stuck, feel free to reach out on Twitter @jntrcs.
