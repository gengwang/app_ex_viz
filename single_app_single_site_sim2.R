source('./utils/ts.R')

qual.ts <- ts_dt_data(
  start = ISOdatetime(2020,1,30,0,0,0), 
  end = ISOdatetime(2020,2,1,0,0,0), 
  by=(60*5)
)

# manually generate some qualitative KPIs for 2 days in an interval of 5 mins
n <- length(qual.ts)
qual.score <- seq_health_scores(size = n, range = 1)
qual.usage <- ran_jump(size=n, mean = 15)
qual.client_count <- ran_jump(size=n, mean = 300) %>% as.integer()
qual.throughput <- qual.usage / 5
qual.packet_loss <- ran_jump(size=n, mean = 0.05, jump = 0.001, noise = 0.001, variance = 0.001)
qual.latency <- ran_jump(size=n, mean = 500, seed = 100) %>% as.integer()
qual.latency_breakdowns <- qual.latency %>% lapply(shatter)

t_b <- qual.latency_breakdowns %>% unlist()
qual.latency.lan <- t_b[c(T, F, F)]
qual.latency.wan <- t_b[c(F, T, F)]
qual.latency.app <- t_b[c(F, F, T)]
# ggplot(data = tibble(x = qual.ts, latency = qual.latency, usage = qual.usage ), aes(x=x))+
#       geom_line(aes(y=latency, color=latency))+
#       geom_line(aes(y=usage, color=usage))

qual_kpis <- tibble(
  ts = qual.ts,
  score = qual.score,
  usage = qual.usage,
  client_count = qual.client_count,
  throughput = qual.throughput,
  packet_loss = qual.packet_loss,
  latency_lan = qual.latency.lan,
  latency_wan = qual.latency.wan,
  latency_app = qual.latency.app
)

save_json_file(qual_kpis, './assets/001.json')
