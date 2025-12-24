


#' remove the duplicates of file analysis by AI.
#' @description
#' which data would be retained depends on time of analysis and state of analysis.
#' if analysised already! retain the first datetime of analysised.
#' if analysising or not analysis yet! retain the last datetime of initiate analysis.
#' @param dt data of analysis, data.table
#' @param rpcols columns which have repated lines.
anaDedup <- function(dt, rpcols = c('file_id', 'anatype')){

  dw = copy(dt)

  if('No.' %in% names(dw))
    dw[, c('No.') := NULL]

  dw = unique(dw)

  #引入辅助列auxiliary，标识是否分析完成
  dw[, ax1 := ifelse(astat == '分析完成' | astat == 5, T, F)]
  #一个文件在一种分析中，只要有一次完成，即视为有完成的
  dw[, ax2 := any(ax1), by = rpcols]

  #分析完成的记录（去掉其中未成功的）
  susc = dw[ax2 == T & ax1 == T]
  susc[, c('ax1', 'ax2') := NULL]

  #未分析完成的记录
  unsc = dw[ax2 == F]
  unsc[, c('ax1', 'ax2') := NULL]


  #分析完成数据去重，以首次完成为准
  #一个文件一种分析中的最小完成时间
  if(nrow(susc) > 0){
    susc = deDups(susc, rpcols, c('comptime'), ori = 0)
  }else
    susc = NULL

  #未分析完成数据去重据，以最新分析时间为准
  #一个文件一种分析中的最大开始时间
  if(nrow(unsc) > 0){
    unsc = deDups(unsc, rpcols, c('comptime'), ori = 1)
  }else
    unsc = NULL

  dw = rbind(susc, unsc)

  return(dw)
}
