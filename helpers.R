
getCtx = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  
  token = query[["token"]]
  taskId = query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx = tercenCtx(taskId=taskId, authToken=token)
  
  # dev
  # ctx = tercenCtx()
  
  return(ctx)
}

getValues <- function(session) {
  
  ctx <- getCtx(session)
  
  df <- ctx %>% select(.x, .y, .ri, .ci) %>%
    group_by(.ri)
  
  factor_colors <- 0
  factor_shapes <- 0
  
  if (length(ctx$colors) >=1)
    factor_colors <- ctx$select(ctx$colors[[1]])[[1]]
  if (length(ctx$colors) > 1)
    factor_shapes <- ctx$select(ctx$colors[[2]])[[1]]
  
  if (! all(is.numeric(factor_colors)))
    factor_colors = as.factor(factor_colors)
  if (! all(is.numeric(factor_shapes)))
    factor_shapes = as.factor(factor_shapes)
  
  
  tcn.labels <- NA
  sizes = NA
  if (length(ctx$labels))
    tcn.labels <- ctx$select(ctx$labels[[1]])[[1]]
  if (!all(is.numeric(tcn.labels))) {
    labels = as.factor(tcn.labels)
  } else {
    sizes = tcn.labels
  }
  
  df = df %>% data.frame(labels, factor_colors, factor_shapes, sizes)
  
  cnames = ctx$cselect()[[1]]
  
  rnames = ctx$rselect()[[1]]
  df = df %>%
    mutate(factor_colors = factor_colors, factor_shapes = factor_shapes, labels = labels) %>%
    left_join(data.frame(.ri = 0:(length(rnames) - 1), rnames), by = ".ri") %>%
    left_join(data.frame(.ci = 0:(length(cnames) - 1), cnames), by = ".ci")
  
  df$factor_colors <- factor(df$factor_colors, levels = levels(df$factor_colors))
  df$factor_shapes <- factor(df$factor_shapes, levels = levels(df$factor_shapes))
  
  return(df)
}

create_colors_vector <- function(no_color_factors) {
  if (no_color_factors > 74) {
    qual_col_pals = brewer.pal.info
  } else {
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  }
  
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  return(col_vector)
}

create_custom_biexp_scale <- function(pos_decades, neg_decades, width_basis) {
  custom_biexp_trans <- flowjo_biexp(pos = pos_decades,
                                     neg = neg_decades, 
                                     widthBasis = width_basis)
  custom_biexp_inv_trans <- flowjo_biexp(pos = pos_decades, 
                                         neg = neg_decades, 
                                         widthBasis = width_basis, 
                                         inverse = TRUE)
  
  custom_biexp_scale <- scales::trans_new(name = 'custom biexponential',
                                          transform = custom_biexp_trans,
                                          inverse = custom_biexp_inv_trans)
  
  return(custom_biexp_scale)
}

create_custom_logicle_scale <- function(w, t, m, a) {
  
  custom_logicle_trans <- logicleTransform(w = w,
                                           t = t,
                                           m = m,
                                           a = a)
  
  custom_logicle_inv_trans <- inverseLogicleTransform(trans = custom_logicle_trans)
  
  custom_logicle_scale <- scales::trans_new(name = 'custom logicle',
                                            transform = custom_logicle_trans,
                                            inverse = custom_logicle_inv_trans)
  
  return(custom_logicle_scale)
}

custom_logicle_breaks <- function(x) {
  
  rng.raw <- range(x, na.rm = TRUE)

  v_min_max = c(minimum = x[1], 
                maximum = x[length(x)])
  
  log_min_max = c()
  for (i in c("minimum", "maximum")) {
    value = v_min_max[i]
    if ( i == "minimum" && sign(value) == 1 ) {
      log_min_max[i] = sign(value)*floor(log10(abs(value)))
    } else {
      log_min_max[i] = sign(value)*ceiling(log10(abs(value)))
    }
  }
  power = log_min_max["minimum"]:log_min_max["maximum"]
  sn = sign(power)
  pow = abs(power)
  
  decades = sn*10^pow; # node that decades includes 0. e.g.: -100, -10, -1, 0, 1, 10.
  n_decades = length(decades)
  n_ticks = (n_decades-1)*9 + 1 + 2*sum(decades==0) #if we have 0 included in our decades, add 2 to the number of ticks because we will tick at -1 and 1
  obj.Tick = rep(0, n_ticks)
  tick_index = 1
  previous_labeled_decade=NA
  
  for(k in 1:n_decades) {
    if (k == n_decades) {
      # write Tick for final decade
      obj.Tick[tick_index] = decades[k]
      # Fill any subsequent ticks
      # which may be labelled '' so that the Tick vector is
      # monotonically increasing;
      if (tick_index < n_ticks) {
        obj.Tick[(tick_index+1):length(obj.Tick)] = seq(obj.Tick[tick_index]+0.1, 
                                                        obj.Tick[tick_index]+0.2, 
                                                        length.out = n_ticks - tick_index)
      }
      break;
    }
    
    # write Tick for this decade in 9 increments if the ends of
    # the decades are powers of 10 increments if the right hand
    # end of the gap is 0 (i.e.10^{-inf})
    if (decades[k+1] == 0) {
      n_increments = 11
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else if (decades[k]==0) {
      n_increments = 9
      lhs = 1
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else {
      n_increments = 9
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    }
    #obj.Tick[tick_index:(tick_index+n_increments-1)] = trans.fun(seq(from = lhs, to = rhs, length.out = n_increments))
    obj.Tick[tick_index:(tick_index+n_increments-1)] = seq(from = lhs, to = rhs, length.out = n_increments)
    
    tick_index = tick_index + n_increments;
  }
  
  return(obj.Tick)
}

break_transform <- function(breaks, transformation)
{
  if ((transformation == "biexponential") || (transformation == "logicle")) {
    x.breaks <- custom_logicle_breaks(breaks)
  } else {
    x.breaks <- breaks
  }
  return(x.breaks)
}

custom_log10 = function(tick) {
  if ( tick == 0) {
    tick.lbl = "0"
  } else { 
    pow <- log10(abs(tick))
    lbl <- paste0(sign(tick)*10, "^", pow)
    tick.lbl = parse(text = lbl)
  }
  return(tick.lbl)
}

custom_tick_labels <- function(breaks) {
  
  labelled_ticks <- c(sapply(seq(14, 1, -1), function(x) -10^x), 0, sapply(seq(1, 14, 1), function(x) 10^x))
  lbls <- sapply(breaks, custom_log10)
  lbls[! breaks %in% labelled_ticks] = ""
  
  return(lbls)
}

theme_fcs <- function() {
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1),
    axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
    axis.text.y = element_text(margin = margin(r = .3, unit = "cm"))
  )
} 
  


is_ten <- function(x){
  
  while( x > 10 ){
    x <- x %/% 10
  }
  
  return((x %% 10)==0)
}


is_five <- function(x){
  
  while( x > 10 ){
    x <- x %/% 10
  }
  
  return((x %% 5)==0)
}
