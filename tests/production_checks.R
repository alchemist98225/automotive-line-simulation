source("app/R/model.R")
base<-function() {
 p<-default_params();p$n_replications<-2L;p$warmup_min<-60;p$shift_length_min<-120;p$n_shifts<-3L
 p$stations<-p$stations %>% mutate(dist="deterministic",mean_min=c(1,10,1,1),load_min=0,unload_min=0,
   mtbf_min=Inf,mttr_min=0,task_interval_min=Inf,buffer_before=0)
 p$operators<-tibble(pool=character(),count=integer());p$stations$operator_pool<-NA_character_
 p$arrival_mean_min<-1;p$order_spacing<-"fixed";p$entry_buffer<-1L;p$release_policy<-"space"
 p
}
p<-base();r<-run_line_sim(p)
stopifnot(r$params$sim_length_min==420,nrow(r$shift_summaries)==6,
  all(r$shift_summaries$end-r$shift_summaries$start==120))
# Complete shifts partition the observation period and conserve vehicles/orders.
stopifnot(all((r$shift_summaries %>% group_by(replication) %>% summarise(n=sum(n_completed),.groups="drop"))$n==r$summaries$n_completed),
 all(with(r$shift_summaries,start_wip+n_released-n_completed==end_wip)),
 all(with(r$shift_summaries,start_backlog+orders_due-n_released==end_backlog)))
# Queue slots include waiting positions only, not the bays. Disregard simultaneous zero-duration transitions.
mon<-r$resources_ts %>% group_by(replication,resource) %>% mutate(dt=lead(time,default=420)-time) %>% filter(dt>1e-9)
stopifnot(all(mon$queue[mon$resource!="body_shop"]==0),all(mon$queue[mon$resource=="body_shop"]<=1),
  all(r$release_timeseries$unreleased_backlog>=0),max(r$station_waits$minutes[r$station_waits$type=="blocked"])>0)
# Compare release-all against finite entrance and WIP-limited admission.
scheduled<-p;scheduled$release_policy<-"scheduled";a<-run_line_sim(scheduled)
stopifnot(all(a$summaries$end_unreleased_backlog==0),all(a$summaries$end_wip>r$summaries$end_wip))
limited<-p;limited$release_policy<-"conwip";limited$wip_limit<-3L;b<-run_line_sim(limited)
event_times<-sort(unique(na.omit(c(b$orders$release_time,b$orders$end_time[b$orders$finished]))))
for(id in unique(b$orders$replication)) {
 bo<-b$orders[b$orders$replication==id,]
 event_wip<-findInterval(event_times,sort(na.omit(bo$release_time)))-findInterval(event_times,sort(bo$end_time[bo$finished]))
 stopifnot(all(event_wip<=3),all(event_wip>=0))
}
stopifnot(max(b$release_timeseries$wip)<=3,all(b$summaries$end_unreleased_backlog>0))
# Changing buffers preserves throughput conservation and restricts actual waiting.
buffered<-p;buffered$stations$buffer_before<-2L;c<-run_line_sim(buffered)
mon<-c$resources_ts %>% group_by(replication,resource) %>% mutate(dt=lead(time,default=420)-time) %>% filter(dt>1e-9)
stopifnot(all(mon$queue[mon$resource!="body_shop"]<=2))
# 24h settling + twenty 8h shifts; runs are separate from shifts.
month<-base();month$warmup_min<-1440;month$shift_length_min<-480;month$n_shifts<-20L;month$n_replications<-1L
m<-run_line_sim(month)
stopifnot(m$params$sim_length_min==11040,nrow(m$shift_summaries)==20,
 sum(m$shift_summaries$n_completed)==m$summaries$n_completed,
 isTRUE(all.equal(sum(m$shift_summaries$avg_wip*480)/9600,m$summaries$avg_wip)))
# No vehicles complete: still return zero rows/series coherently for every run/station.
z<-base();z$shift_length_min<-.001;z$n_shifts<-1L;z$warmup_min<-0;z$order_spacing<-"exponential"
z$arrival_mean_min<-1e6;z<-run_line_sim(z)
stopifnot(nrow(z$queue_timeseries)==2*4*2,all(z$queue_timeseries$server==0),all(z$shift_summaries$n_completed==0))
# Repair waiting is part of total downtime, including failures not repaired by run end.
f<-base();f$shift_length_min<-480;f$n_shifts<-1L;f$warmup_min<-0
f$stations$mtbf_min<-5;f$stations$mttr_min<-60;fr<-run_line_sim(f)
stopifnot(sum(fr$downtime_by_rep$waiting_repair_min)>0,
 all(fr$downtime_by_rep$total_downtime_min>=fr$downtime_by_rep$downtime_min),
 all(fr$downtime_by_rep$total_downtime_fraction<=1+1e-9))
# Run end censors technician waiting, not missing failures from the ledger.
reps<-tibble(replication=1L,seed=1L);s<-base();s<-normalize_params(s);s$stations<-s$stations[1,];s$stations$capacity<-1
failures<-tibble(replication=1L,station="body_shop",time=c(10,100),failure_id=1:2)
repairs<-tibble(replication=1L,station="body_shop",time=30,duration=5,failure_id=1L)
d<-build_downtime(repairs,reps,s,failures)
stopifnot(d$downtime_min==0,d$total_downtime_min==320,d$waiting_repair_min==320)
# Admission never duplicates or drops planned orders.
stopifnot(!anyDuplicated(r$orders[c("replication","name")]),
 all(r$orders$release_time>=r$orders$order_time,na.rm=TRUE))
cat("Production plan, blocking, admission and conservation checks passed\n")
