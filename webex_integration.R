source('./utils/ts.R')

size = 24*2+1

# get every 2 hrs?
qual.ts <- ts_dt_data(
  start = ISOdatetime(2020,1,30,0,0,0), 
  end = ISOdatetime(2020,2,1,0,0,0), 
  by = (60*5*12)
)

qual.audio <- seq_health_scores(size = size, start = sample(1:10, 1))
qual.video <- seq_health_scores(size = size, start = sample(1:10, 1))
qual.share <- seq_health_scores(size = size, start = sample(1:10, 1))

qual_kpis <- tibble(
  ts = qual.ts,
  audio = qual.audio,
  video = qual.video,
  share = qual.share
)

qual_kpis_spline <- tibble(
  audio = spline(qual.audio),
  video = spline(qual.video),
  share = spline(qual.share)
)

spline(qual.share)

save_json_file(qual_kpis, './assets/webex_kpi_ts.json')
write.csv(qual_kpis, './assets/webex_kpi_ts.csv')

ggplot(qual_kpis) +
  geom_smooth(aes( y = audio,  x = ts), color = "#4960D6f6", method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  geom_line( aes( y = audio,  x = ts), color = "#4960D6", size = 1 ) +
  geom_line( aes( y = video,  x = ts), color = "#8C91BD", size = 1 ) +
  geom_line( aes( y = share,  x = ts), color = "#F384F5", size = 1 ) 
