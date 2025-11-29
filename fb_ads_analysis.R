# Facebook Ads Performance Review Tool
# Practical analysis for ROAS and CTR improvement
# Created by: Marketing Analytics
# Last updated: December 2024

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(jsonlite)
library(zoo)


analysis_settings <- list(
  data_source = "data/dataset.csv",
  performance_limits = list(
    low_ctr_limit = 0.5,
    roas_drop_threshold = 0.2,
    minimum_confidence = 0.7
  ),
  system_settings = list(
    max_attempts = 3,
    sample_size = 1000
  )
)


make_analysis_plan <- function(user_question) {
  cat("Creating analysis plan for:", user_question, "\n")
  
  question_lower <- tolower(user_question)
  
  if (grepl("roas.*drop", question_lower) || grepl("drop.*roas", question_lower)) {
    plan_steps <- list(
      list(step = "get_data", description = "Load and clean the data", agent = "data_handler", needs = c()),
      list(step = "check_roas", description = "Look for ROAS drops and patterns", agent = "data_handler", needs = c("get_data")),
      list(step = "find_causes", description = "Identify why performance changed", agent = "insight_generator", needs = c("get_data", "check_roas")),
      list(step = "test_ideas", description = "Check if our theories are correct", agent = "theory_tester", needs = c("find_causes")),
      list(step = "review_creatives", description = "Look at ad creative performance", agent = "insight_generator", needs = c("get_data")),
      list(step = "make_new_creatives", description = "Create better ad concepts", agent = "creative_maker", needs = c("review_creatives"))
    )
    plan_reasoning <- "ROAS drop analysis needs data checking, cause finding, validation, and new creative ideas."
  } else if (grepl("ctr", question_lower)) {
    plan_steps <- list(
      list(step = "get_data", description = "Load dataset", agent = "data_handler", needs = c()),
      list(step = "check_ctr", description = "Analyze click-through rates", agent = "insight_generator", needs = c("get_data")),
      list(step = "make_new_creatives", description = "Design better ads", agent = "creative_maker", needs = c("check_ctr"))
    )
    plan_reasoning <- "CTR analysis focuses on engagement metrics and ad improvements."
  } else {
    plan_steps <- list(
      list(step = "get_data", description = "Load dataset", agent = "data_handler", needs = c()),
      list(step = "full_review", description = "Complete performance check", agent = "insight_generator", needs = c("get_data"))
    )
    plan_reasoning <- "General performance review covering main metrics."
  }
  
  final_plan <- list(
    steps = plan_steps,
    reasoning = plan_reasoning,
    original_question = user_question
  )
  
  cat("Analysis plan created with", length(plan_steps), "steps\n")
  return(final_plan)
}

# Step 2: Handle data loading and preparation
load_and_prepare_data <- function(file_path) {
  cat("Loading data from:", file_path, "\n")
  
  if (!file.exists(file_path)) {
    stop("Cannot find data file at: ", file_path)
  }
  
  ads_data <- read.csv(file_path, stringsAsFactors = FALSE)
  cat("Loaded", nrow(ads_data), "records with", ncol(ads_data), "columns\n")
  
  data_columns <- colnames(ads_data)
  cat("Data columns:", paste(data_columns, collapse = ", "), "\n")
  
  if ("date" %in% data_columns) {
    ads_data$date <- as.Date(ads_data$date)
    cat("Fixed date format\n")
  }
  
  return(ads_data)
}

create_data_summary <- function(dataset) {
  cat("Creating data summary\n")
  
  summary_info <- list(
    basics = list(),
    metrics = list(),
    categories = list()
  )
  
  summary_info$basics$total_records <- nrow(dataset)
  if ("date" %in% colnames(dataset)) {
    summary_info$basics$date_range <- list(
      start = as.character(min(dataset$date)),
      end = as.character(max(dataset$date))
    )
  }
  
  if ("campaign_name" %in% colnames(dataset)) {
    summary_info$basics$campaign_total <- length(unique(dataset$campaign_name))
  }
  
  number_columns <- c("spend", "revenue", "impressions", "clicks", "purchases")
  for (col in number_columns) {
    if (col %in% colnames(dataset)) {
      summary_info$basics[[paste("total_", col, sep = "")]] <- sum(dataset[[col]], na.rm = TRUE)
      summary_info$basics[[paste("avg_", col, sep = "")]] <- mean(dataset[[col]], na.rm = TRUE)
    }
  }
  
  if ("roas" %in% colnames(dataset)) {
    summary_info$metrics$avg_roas <- mean(dataset$roas, na.rm = TRUE)
  }
  if ("ctr" %in% colnames(dataset)) {
    summary_info$metrics$avg_ctr <- mean(dataset$ctr, na.rm = TRUE)
  }
  
  category_columns <- c("creative_type", "audience_type", "platform", "country")
  for (col in category_columns) {
    if (col %in% colnames(dataset)) {
      summary_info$categories[[col]] <- as.list(table(dataset[[col]]))
    }
  }
  
  cat("Data summary completed\n")
  return(summary_info)
}

analyze_roas_changes <- function(dataset) {
  cat("Checking ROAS trends and drops\n")
  
  if (!("date" %in% colnames(dataset) && "roas" %in% colnames(dataset))) {
    warning("Need date and ROAS columns for analysis")
    return(list(error = "Missing required columns"))
  }
  
  daily_roas <- dataset %>%
    group_by(date) %>%
    summarise(
      day_roas = mean(roas, na.rm = TRUE),
      day_spend = sum(spend, na.rm = TRUE),
      day_revenue = sum(revenue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date)
  
  if (nrow(daily_roas) >= 7) {
    daily_roas$week_avg <- zoo::rollmean(daily_roas$day_roas, 7, fill = NA, align = "right")
    daily_roas$pct_change <- c(NA, diff(daily_roas$week_avg) / head(daily_roas$week_avg, -1))
  } else {
    daily_roas$week_avg <- daily_roas$day_roas
    daily_roas$pct_change <- 0
  }
  
  big_drops <- daily_roas %>%
    filter(pct_change < -analysis_settings$performance_limits$roas_drop_threshold) %>%
    na.omit()
  
  results <- list(
    daily_data = daily_roas,
    significant_drops = big_drops,
    summary = list(
      days_analyzed = nrow(daily_roas),
      drop_count = nrow(big_drops),
      worst_drop_pct = if (nrow(big_drops) > 0) min(big_drops$pct_change, na.rm = TRUE) else 0,
      overall_roas_avg = mean(daily_roas$day_roas, na.rm = TRUE)
    )
  )
  
  cat("Found", nrow(big_drops), "significant ROAS drops\n")
  return(results)
}

# Step 3: Generate insights from the data
find_insights <- function(data_summary, roas_analysis) {
  cat("Looking for performance insights\n")
  
  insights <- list()
  insight_num <- 1
  
  if (length(data_summary$categories$audience_type) > 0) {
    audience_counts <- unlist(data_summary$categories$audience_type)
    concentration <- max(audience_counts) / sum(audience_counts)
    
    if (concentration > 0.6) {
      main_audience <- names(which.max(audience_counts))
      insights[[insight_num]] <- list(
        description = paste("Possible audience fatigue in '", main_audience, "' group", sep = ""),
        confidence = 0.75,
        evidence = list(paste("Audience focus: ", round(concentration * 100, 1), "%", sep = "")),
        testable = TRUE,
        category = "audience_fatigue"
      )
      insight_num <- insight_num + 1
    }
  }
  
  if (!is.null(roas_analysis$significant_drops) && nrow(roas_analysis$significant_drops) > 0) {
    insights[[insight_num]] <- list(
      description = "Ad creative fatigue hurting engagement",
      confidence = 0.7,
      evidence = list(paste("Found ", nrow(roas_analysis$significant_drops), " ROAS drops", sep = "")),
      testable = TRUE,
      category = "creative_fatigue"
    )
    insight_num <- insight_num + 1
  }
  
  if (length(data_summary$categories$creative_type) == 1) {
    creative_style <- names(data_summary$categories$creative_type)[1]
    insights[[insight_num]] <- list(
      description = paste("Limited ad variety - only using '", creative_style, "' format", sep = ""),
      confidence = 0.65,
      evidence = list(paste("Single creative type: ", creative_style, sep = "")),
      testable = TRUE,
      category = "creative_variety"
    )
    insight_num <- insight_num + 1
  }
  
  cat("Created", length(insights), "performance insights\n")
  return(insights)
}

find_problem_campaigns <- function(dataset) {
  cat("Finding campaigns with performance issues\n")
  
  ctr_min <- analysis_settings$performance_limits$low_ctr_limit
  
  needed_columns <- c("campaign_name", "ctr", "roas", "spend", "revenue", "impressions", "clicks", "purchases")
  missing_columns <- setdiff(needed_columns, colnames(dataset))
  
  if (length(missing_columns) > 0) {
    warning("Missing data for campaign analysis: ", paste(missing_columns, collapse = ", "))
    return(list(error = paste("Missing:", paste(missing_columns, collapse = ", "))))
  }
  
  campaign_metrics <- dataset %>%
    group_by(campaign_name) %>%
    summarise(
      avg_ctr = mean(ctr, na.rm = TRUE),
      avg_roas = mean(roas, na.rm = TRUE),
      total_spend = sum(spend, na.rm = TRUE),
      total_revenue = sum(revenue, na.rm = TRUE),
      total_impressions = sum(impressions, na.rm = TRUE),
      total_clicks = sum(clicks, na.rm = TRUE),
      total_purchases = sum(purchases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      calculated_ctr = total_clicks / total_impressions,
      performance_score = avg_ctr * avg_roas
    ) %>%
    filter(total_spend > 0)
  
  bad_ctr_campaigns <- campaign_metrics %>%
    filter(avg_ctr < ctr_min) %>%
    arrange(avg_ctr)
  
  bad_roas_campaigns <- campaign_metrics %>%
    filter(avg_roas < 1.0) %>%
    arrange(avg_roas)
  
  findings <- list(
    all_campaigns = campaign_metrics,
    low_ctr_campaigns = bad_ctr_campaigns,
    low_roas_campaigns = bad_roas_campaigns,
    summary_stats = list(
      campaigns_checked = nrow(campaign_metrics),
      low_ctr_count = nrow(bad_ctr_campaigns),
      low_roas_count = nrow(bad_roas_campaigns),
      overall_avg_ctr = mean(campaign_metrics$avg_ctr, na.rm = TRUE),
      overall_avg_roas = mean(campaign_metrics$avg_roas, na.rm = TRUE)
    )
  )
  
  cat("Found", nrow(bad_ctr_campaigns), "campaigns with low CTR\n")
  return(findings)
}

# Step 4: Test our theories about performance issues
test_our_ideas <- function(idea, dataset, data_summary) {
  cat("Testing idea -", idea$description, "\n")
  
  test_results <- list(
    idea_text = idea$description,
    start_confidence = idea$confidence,
    tests_run = list()
  )
  
  if (idea$category == "audience_fatigue") {
    test_outcome <- test_audience_fatigue(dataset, data_summary)
  } else if (idea$category == "creative_fatigue") {
    test_outcome <- test_creative_fatigue(dataset)
  } else if (idea$category == "creative_variety") {
    test_outcome <- test_creative_variety(data_summary)
  } else {
    test_outcome <- list(valid = FALSE, confidence = 0.3, details = list(), conclusion = "Idea type not supported")
  }
  
  test_results$valid <- test_outcome$valid
  test_results$final_confidence <- test_outcome$confidence
  test_results$tests_run <- test_outcome$details
  test_results$conclusion <- test_outcome$conclusion
  
  status <- if (test_results$valid) "CONFIRMED" else "NOT CONFIRMED"
  cat("Testing result:", status, "with confidence:", round(test_results$final_confidence, 2), "\n")
  
  return(test_results)
}

test_audience_fatigue <- function(dataset, data_summary) {
  test_details <- list()
  
  if (length(data_summary$categories$audience_type) == 0) {
    return(list(valid = FALSE, confidence = 0.3, details = list(), conclusion = "No audience data available"))
  }
  
  audience_numbers <- unlist(data_summary$categories$audience_type)
  concentration <- max(audience_numbers) / sum(audience_numbers)
  
  test_details$concentration_check <- list(
    test_type = "Audience Concentration",
    result = paste("Main audience segment: ", round(concentration * 100, 1), "%", sep = "")
  )
  
  if ("audience_type" %in% colnames(dataset) && "roas" %in% colnames(dataset)) {
    audience_performance <- dataset %>%
      group_by(audience_type) %>%
      summarise(
        segment_roas = mean(roas, na.rm = TRUE),
        segment_ctr = mean(ctr, na.rm = TRUE),
        segment_spend = sum(spend, na.rm = TRUE),
        .groups = "drop"
      )
    
    main_audience <- names(which.max(unlist(data_summary$categories$audience_type)))
    main_performance <- audience_performance %>% 
      filter(audience_type == main_audience)
    
    other_audience_performance <- audience_performance %>% 
      filter(audience_type != main_audience)
    
    if (nrow(main_performance) > 0 && nrow(other_audience_performance) > 0) {
      avg_other_roas <- mean(other_audience_performance$segment_roas, na.rm = TRUE)
      performance_gap <- main_performance$segment_roas / avg_other_roas
      
      test_details$performance_check <- list(
        test_type = "Performance Comparison",
        result = paste("Main audience ROAS is ", round(performance_gap, 2), "x other audiences", sep = "")
      )
      
      is_valid <- concentration > 0.6 && performance_gap < 0.8
      confidence_level <- min(0.9, concentration * (1.1 - performance_gap))
    } else {
      is_valid <- concentration > 0.6
      confidence_level <- concentration
    }
  } else {
    is_valid <- concentration > 0.6
    confidence_level <- concentration
  }
  
  conclusion_text <- if (is_valid) {
    "Good evidence for audience fatigue"
  } else {
    "Not enough evidence for audience fatigue"
  }
  
  return(list(
    valid = is_valid,
    confidence = confidence_level,
    details = test_details,
    conclusion = conclusion_text
  ))
}

test_creative_fatigue <- function(dataset) {
  test_details <- list()
  
  if (!("date" %in% colnames(dataset) && "creative_type" %in% colnames(dataset))) {
    return(list(
      valid = FALSE,
      confidence = 0.3,
      details = list(),
      conclusion = "Need date and creative type data"
    ))
  }
  
  creative_trends <- dataset %>%
    group_by(date, creative_type) %>%
    summarise(
      daily_roas = mean(roas, na.rm = TRUE),
      daily_ctr = mean(ctr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(creative_type) %>%
    mutate(
      days_since_start = as.numeric(date - min(date))
    )
  
  if (nrow(creative_trends) >= 5) {
    performance_over_time <- creative_trends %>%
      group_by(creative_type) %>%
      filter(n() >= 3) %>%
      summarise(
        time_corr = if (var(days_since_start) > 0) cor(days_since_start, daily_roas, use = "complete.obs") else 0,
        .groups = "drop"
      )
    
    avg_correlation <- mean(performance_over_time$time_corr, na.rm = TRUE)
    
    test_details$trend_analysis <- list(
      test_type = "Performance Over Time",
      result = paste("Average correlation: ", round(avg_correlation, 3), sep = "")
    )
    
    is_valid <- avg_correlation < -0.2
    confidence_level <- max(0.3, min(0.8, abs(avg_correlation) * 2))
  } else {
    is_valid <- FALSE
    confidence_level <- 0.3
  }
  
  conclusion_text <- if (is_valid) {
    "Evidence suggests creative fatigue"
  } else {
    "No strong creative fatigue evidence"
  }
  
  return(list(
    valid = is_valid,
    confidence = confidence_level,
    details = test_details,
    conclusion = conclusion_text
  ))
}

test_creative_variety <- function(data_summary) {
  test_details <- list()
  
  creative_types_count <- length(data_summary$categories$creative_type)
  
  test_details$variety_check <- list(
    test_type = "Creative Type Count",
    result = paste(creative_types_count, "different creative types used", sep = " ")
  )
  
  is_valid <- creative_types_count == 1
  confidence_level <- if (is_valid) 0.8 else 0.3
  
  conclusion_text <- if (is_valid) {
    "Confirmed limited creative variety"
  } else {
    "Good creative variety present"
  }
  
  return(list(
    valid = is_valid,
    confidence = confidence_level,
    details = test_details,
    conclusion = conclusion_text
  ))
}

# Step 5: Create new ad concepts for underperforming campaigns
create_ad_concepts <- function(problem_campaigns, dataset, data_summary) {
  cat("Creating new ad concepts\n")
  
  new_concepts <- list()
  
  if (!is.null(problem_campaigns$error)) {
    cat("Cannot create concepts -", problem_campaigns$error, "\n")
    return(list())
  }
  
  if (nrow(problem_campaigns$low_ctr_campaigns) == 0) {
    cat("No problem campaigns found\n")
    return(list())
  }
  
  good_ads_patterns <- find_winning_ads(dataset)
  
  campaigns_to_work_on <- min(3, nrow(problem_campaigns$low_ctr_campaigns))
  
  for (i in 1:campaigns_to_work_on) {
    campaign <- problem_campaigns$low_ctr_campaigns[i, ]
    campaign_name <- campaign$campaign_name
    
    cat("Creating concepts for:", campaign_name, "\n")
    
    campaign_ideas <- make_campaign_ideas(
      campaign, 
      good_ads_patterns, 
      dataset,
      data_summary
    )
    
    new_concepts <- c(new_concepts, campaign_ideas)
  }
  
  cat("Created", length(new_concepts), "new ad concepts\n")
  return(new_concepts)
}

find_winning_ads <- function(dataset) {
  high_ctr_level <- quantile(dataset$ctr, 0.75, na.rm = TRUE)
  high_roas_level <- quantile(dataset$roas, 0.75, na.rm = TRUE)
  
  top_ads <- dataset %>%
    filter(ctr > high_ctr_level | roas > high_roas_level)
  
  winning_patterns <- list()
  
  if ("creative_type" %in% colnames(top_ads)) {
    top_formats <- names(sort(table(top_ads$creative_type), decreasing = TRUE))
    winning_patterns$good_formats <- head(top_formats, min(3, length(top_formats)))
  }
  
  if ("creative_message" %in% colnames(top_ads)) {
    messages <- top_ads$creative_message[!is.na(top_ads$creative_message)]
    if (length(messages) > 0) {
      winning_patterns$message_examples <- head(messages, min(5, length(messages)))
    }
  }
  
  if ("audience_type" %in% colnames(top_ads)) {
    top_audiences <- names(sort(table(top_ads$audience_type), decreasing = TRUE))
    winning_patterns$good_audiences <- head(top_audiences, min(3, length(top_audiences)))
  }
  
  return(winning_patterns)
}

make_campaign_ideas <- function(campaign, winning_patterns, dataset, data_summary) {
  campaign_ideas <- list()
  
  base_name <- gsub("_", " ", campaign$campaign_name)
  base_name <- gsub("campaign", "", base_name, ignore.case = TRUE)
  base_name <- gsub("undergarments?", "", base_name, ignore.case = TRUE)
  base_name <- trimws(base_name)
  
  if (nchar(base_name) == 0) {
    base_name <- "Undergarments"
  }
  
  headline_choices <- c(
    "Experience Ultimate Comfort with Our {product}",
    "Premium Quality {product} - Notice the Difference",
    "Innovative {product} for Daily Comfort",
    "Discover Our Popular {product} Collection",
    "Soft, Supportive {product} You'll Appreciate",
    "Everyday Essentials: {product} Line",
    "Upgrade Your {product} Experience",
    "Luxury {product} at Accessible Prices"
  )
  
  message_choices <- c(
    "Discover exceptional comfort with our premium {product}. Crafted for your daily wear with superior materials and perfect fit.",
    "Transform your everyday experience with our comfortable {product}. Join numerous satisfied customers who value our quality and comfort.",
    "Why settle for less? Our {product} provides ideal support and contemporary style. Limited availability - act now!",
    "Find the perfect combination of style and comfort with our {product} collection. Your complete satisfaction is our commitment.",
    "Elevate your basics with our premium {product}. Engineered for comfort, designed for durability, appreciated by customers.",
    "Notice the improvement with our carefully designed {product}. Experience the quality that distinguishes us from competitors."
  )
  
  cta_choices <- c("Shop Now", "Find Your Size", "Purchase Now", "Explore", "Get Yours", 
                   "Browse Collection", "Order Today", "View Options", "Experience Comfort", "Shop Basics")
  
  for (idea_num in 1:3) {
    chosen_headline <- gsub("\\{product\\}", base_name, sample(headline_choices, 1))
    chosen_message <- gsub("\\{product\\}", base_name, sample(message_choices, 1))
    chosen_cta <- sample(cta_choices, 1)
    
    suggested_format <- if (length(winning_patterns$good_formats) > 0) {
      sample(winning_patterns$good_formats, 1)
    } else {
      "image"
    }
    
    current_ctr <- campaign$avg_ctr
    average_ctr_all <- if (!is.null(data_summary$metrics$avg_ctr)) {
      data_summary$metrics$avg_ctr
    } else {
      mean(dataset$ctr, na.rm = TRUE)
    }
    
    confidence_score <- 0.6 + (0.3 * (1 - current_ctr / average_ctr_all))
    confidence_score <- min(0.9, max(0.5, confidence_score))
    
    new_idea <- list(
      for_campaign = campaign$campaign_name,
      headline = chosen_headline,
      message = chosen_message,
      cta = chosen_cta,
      format = suggested_format,
      reasoning = paste("Current CTR: ", round(current_ctr, 3), 
                        " | Based on analysis of top-performing ads", sep = ""),
      confidence = round(confidence_score, 2),
      improvement_goal = paste("Expected CTR improvement: ", 
                               round((average_ctr_all - current_ctr) * 100, 1), "%", sep = "")
    )
    
    campaign_ideas <- append(campaign_ideas, list(new_idea))
  }
  
  return(campaign_ideas)
}

# Main function to run everything
run_full_analysis <- function(analysis_question = "Analyze ROAS drop") {
  cat("Facebook Ads Performance Analysis Tool\n")
  cat("Analysis Request:", analysis_question, "\n")
  
  if (!dir.exists("analysis_reports")) {
    dir.create("analysis_reports")
    cat("Created analysis_reports folder\n")
  }
  
  cat("\nStep 1: Making Analysis Plan\n")
  analysis_plan <- make_analysis_plan(analysis_question)
  
  cat("\nStep 2: Processing Data\n")
  raw_data <- load_and_prepare_data(analysis_settings$data_source)
  data_summary <- create_data_summary(raw_data)
  
  cat("\nStep 3: ROAS Analysis\n")
  roas_analysis <- analyze_roas_changes(raw_data)
  
  cat("\nStep 4: Finding Insights\n")
  performance_insights <- find_insights(data_summary, roas_analysis)
  campaign_analysis <- find_problem_campaigns(raw_data)
  
  cat("\nStep 5: Testing Ideas\n")
  tested_insights <- list()
  for (i in seq_along(performance_insights)) {
    test_result <- test_our_ideas(performance_insights[[i]], raw_data, data_summary)
    tested_insights <- append(tested_insights, list(test_result))
  }
  
  cat("\nStep 6: Creating New Ad Concepts\n")
  new_ad_concepts <- create_ad_concepts(campaign_analysis, raw_data, data_summary)
  
  final_results <- list(
    analysis_info = list(
      question = analysis_question,
      run_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      data_file = analysis_settings$data_source
    ),
    data_overview = data_summary,
    roas_analysis = roas_analysis,
    tested_insights = tested_insights,
    campaign_performance = campaign_analysis,
    new_ad_concepts = new_ad_concepts
  )
  
  save_analysis_results(final_results)
  
  cat("\nAnalysis Complete\n")
  cat("Check the analysis_reports folder for:\n")
  cat("  - performance_insights.json (findings)\n")
  cat("  - creative_concepts.json (new ad ideas)\n") 
  cat("  - analysis_report.md (full summary)\n")
  
  return(final_results)
}

save_analysis_results <- function(results) {
  run_time <- results$analysis_info$run_time
  
  insights_output <- list(
    info = results$analysis_info,
    data_summary = list(
      records = results$data_overview$basics$total_records,
      campaigns = results$data_overview$basics$campaign_total,
      avg_roas = results$data_overview$metrics$avg_roas,
      avg_ctr = results$data_overview$metrics$avg_ctr
    ),
    roas_findings = results$roas_analysis$summary,
    tested_insights = results$tested_insights
  )
  
  write_json(insights_output, "analysis_reports/performance_insights.json", pretty = TRUE, auto_unbox = TRUE)
  
  creative_output <- list(
    info = list(
      created_time = run_time,
      total_concepts = length(results$new_ad_concepts)
    ),
    ad_concepts = results$new_ad_concepts,
    problem_campaigns = if (!is.null(results$campaign_performance$low_ctr_campaigns)) {
      head(results$campaign_performance$low_ctr_campaigns, 5)
    } else {
      list()
    }
  )
  
  write_json(creative_output, "analysis_reports/creative_concepts.json", pretty = TRUE, auto_unbox = TRUE)
  
  create_full_report(results)
}

create_full_report <- function(results) {
  report_text <- paste0(
    "# Facebook Ads Performance Analysis Report\n",
    "**Report Created:** ", results$analysis_info$run_time, "  \n",
    "**Analysis Focus:** ", results$analysis_info$question, "  \n",
    "**Data Source:** ", results$analysis_info$data_file, "  \n\n",
    
    "## Executive Summary\n\n",
    "This analysis examined Facebook Ads performance data, testing ",
    length(results$tested_insights), " key ideas and creating ",
    length(results$new_ad_concepts), " new ad concepts for improvement.\n\n",
    
    "## Data Overview\n\n",
    "- **Total Records:** ", results$data_overview$basics$total_records, "  \n",
    "- **Campaigns Analyzed:** ", results$data_overview$basics$campaign_total, "  \n",
    "- **Total Ad Spend:** $", format(results$data_overview$basics$total_spend, big.mark = ",", nsmall = 2), "  \n",
    "- **Total Revenue:** $", format(results$data_overview$basics$total_revenue, big.mark = ",", nsmall = 2), "  \n",
    "- **Average ROAS:** ", round(results$data_overview$metrics$avg_roas, 2), "  \n",
    "- **Average CTR:** ", round(results$data_overview$metrics$avg_ctr, 3), "  \n\n",
    
    "## ROAS Analysis\n\n",
    "- **ROAS Drops Found:** ", results$roas_analysis$summary$drop_count, "  \n",
    "- **Largest Drop:** ", if (results$roas_analysis$summary$worst_drop_pct != 0) {
      paste(round(results$roas_analysis$summary$worst_drop_pct * 100, 1), "%", sep = "")
    } else {
      "No major drops"
    }, "  \n",
    "- **Analysis Period:** ", results$roas_analysis$summary$days_analyzed, " days  \n\n",
    
    "## Tested Performance Ideas\n\n"
  )
  
  if (length(results$tested_insights) > 0) {
    for (i in seq_along(results$tested_insights)) {
      insight <- results$tested_insights[[i]]
      status_icon <- if (insight$valid) "✅" else "❌"
      report_text <- paste0(
        report_text,
        "### ", i, ". ", insight$idea_text, "  \n",
        status_icon, " **", if (insight$valid) "CONFIRMED" else "NOT CONFIRMED", "**  \n",
        "- **Confidence:** ", round(insight$final_confidence * 100, 0), "%  \n",
        "- **Conclusion:** ", insight$conclusion, "  \n"
      )
      
      if (length(insight$tests_run) > 0) {
        report_text <- paste0(report_text, "- **Evidence:**  \n")
        for (test in insight$tests_run) {
          if (is.list(test) && !is.null(test$test_type)) {
            report_text <- paste0(
              report_text,
              "  - ", test$test_type, ": ", test$result, "  \n"
            )
          }
        }
      }
      report_text <- paste0(report_text, "  \n")
    }
  } else {
    report_text <- paste0(report_text, "No performance ideas were tested in this analysis.\n\n")
  }
  
  report_text <- paste0(
    report_text,
    "## New Ad Concepts\n\n"
  )
  
  if (length(results$new_ad_concepts) > 0) {
    for (i in seq_along(results$new_ad_concepts)) {
      concept <- results$new_ad_concepts[[i]]
      report_text <- paste0(
        report_text,
        "### ", i, ". Campaign: ", concept$for_campaign, "  \n",
        "- **Headline:** ", concept$headline, "  \n", 
        "- **Message:** ", concept$message, "  \n",
        "- **Call to Action:** ", concept$cta, "  \n",
        "- **Format:** ", concept$format, "  \n",
        "- **Confidence:** ", round(concept$confidence * 100, 0), "%  \n",
        "- **Reasoning:** ", concept$reasoning, "  \n",
        "- **Goal:** ", concept$improvement_goal, "  \n\n"
      )
    }
  } else {
    report_text <- paste0(report_text, "No new ad concepts were created.\n\n")
  }
  
  report_text <- paste0(
    report_text,
    "## Campaign Performance Summary\n\n",
    "- **Campaigns Reviewed:** ", results$campaign_performance$summary_stats$campaigns_checked, "  \n",
    "- **Low CTR Campaigns:** ", results$campaign_performance$summary_stats$low_ctr_count, "  \n", 
    "- **Low ROAS Campaigns:** ", results$campaign_performance$summary_stats$low_roas_count, "  \n",
    "- **Overall Average CTR:** ", round(results$campaign_performance$summary_stats$overall_avg_ctr, 3), "  \n",
    "- **Overall Average ROAS:** ", round(results$campaign_performance$summary_stats$overall_avg_roas, 2), "  \n\n",
    
    "## Recommended Next Steps\n\n",
    "1. **Review Audience Targeting**: Try new audience segments to reduce fatigue\n",
    "2. **Test New Ads**: Implement the new ad concepts for struggling campaigns\n", 
    "3. **Monitor Performance**: Keep tracking ROAS and CTR weekly\n",
    "4. **Run A/B Tests**: Compare new ads against current ones\n",
    "5. **Adjust Budgets**: Move budget to better-performing campaigns\n\n",
    
    "---\n",
   
    
    
    "---\n",
    "*Report created by Facebook Ads Performance Analysis Tool*  \n",
    "*See JSON files for detailed data and additional insights*  \n"
  )
  
  writeLines(report_text, "analysis_reports/analysis_report.md")
  cat("Comprehensive report generated: analysis_reports/analysis_report.md\n")
}

# =============================================================================
# EXECUTION AND USAGE EXAMPLES
# =============================================================================

# Function to run quick analysis
quick_analysis <- function() {
  cat("Running quick Facebook Ads analysis...\n")
  results <- run_full_analysis()
  return(results)
}

# Function to analyze specific issue
analyze_specific_issue <- function(issue_type) {
  if (issue_type == "ctr") {
    return(run_full_analysis("Analyze CTR performance"))
  } else if (issue_type == "roas") {
    return(run_full_analysis("Analyze ROAS drop"))
  } else if (issue_type == "general") {
    return(run_full_analysis("General performance review"))
  } else {
    return(run_full_analysis(issue_type))
  }
}

# Function to get summary statistics
get_performance_summary <- function() {
  cat("Generating performance summary...\n")
  data <- load_and_prepare_data(analysis_settings$data_source)
  summary <- create_data_summary(data)
  
  cat("\n=== PERFORMANCE SUMMARY ===\n")
  cat("Total Records:", summary$basics$total_records, "\n")
  cat("Campaigns:", summary$basics$campaign_total, "\n")
  cat("Total Spend: $", format(summary$basics$total_spend, big.mark = ",", nsmall = 2), "\n", sep = "")
  cat("Total Revenue: $", format(summary$basics$total_revenue, big.mark = ",", nsmall = 2), "\n", sep = "")
  cat("Average ROAS:", round(summary$metrics$avg_roas, 2), "\n")
  cat("Average CTR:", round(summary$metrics$avg_ctr, 3), "\n")
  
  return(summary)
}

# =============================================================================
# MAIN EXECUTION - RUN THE ANALYSIS
# =============================================================================

cat("=============================================\n")
cat("FACEBOOK ADS PERFORMANCE ANALYSIS SYSTEM\n")
cat("=============================================\n")
cat("System ready for analysis\n\n")

cat("Available functions:\n")
cat("1. run_full_analysis() - Complete analysis with default query\n")
cat("2. run_full_analysis('your question') - Custom analysis\n")
cat("3. quick_analysis() - Quick performance check\n")
cat("4. analyze_specific_issue('ctr') - CTR-focused analysis\n")
cat("5. analyze_specific_issue('roas') - ROAS-focused analysis\n")
cat("6. get_performance_summary() - Basic performance stats\n\n")

cat("Example usage:\n")
cat('run_full_analysis("Why is ROAS dropping?")\n')
cat('run_full_analysis("Analyze CTR performance")\n')
cat('run_full_analysis("Find underperforming campaigns")\n\n')

# Uncomment the line below to run the analysis automatically:
# run_full_analysis()

cat("To start analysis, run: run_full_analysis()\n")
cat("Or specify your question: run_full_analysis('your question here')\n")

# =============================================================================
# ERROR HANDLING AND VALIDATION
# =============================================================================

# Validate data file exists
validate_data_file <- function() {
  if (!file.exists(analysis_settings$data_source)) {
    cat("WARNING: Data file not found at:", analysis_settings$data_source, "\n")
    cat("Please check the file path in analysis_settings$data_source\n")
    return(FALSE)
  }
  return(TRUE)
}

# Check required packages
check_packages <- function() {
  required_packages <- c("dplyr", "tidyr", "lubridate", "stringr", "jsonlite", "zoo")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("Missing required packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("Install with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n", sep = "")
    return(FALSE)
  }
  return(TRUE)
}

# Initialize system
initialize_system <- function() {
  cat("Initializing Facebook Ads Analysis System...\n")
  
  # Check packages
  if (!check_packages()) {
    return(FALSE)
  }
  
  # Validate data file
  if (!validate_data_file()) {
    return(FALSE)
  }
  
  # Create reports directory
  if (!dir.exists("analysis_reports")) {
    dir.create("analysis_reports")
    cat("Created analysis_reports directory\n")
  }
  
  cat("System initialized successfully!\n")
  return(TRUE)
}

# Run initialization
if (initialize_system()) {
  cat("\nSystem ready! Use run_full_analysis() to start.\n")
} else {
  cat("\nSystem initialization failed. Please check the warnings above.\n")
}

# =============================================================================
# SAMPLE DATA VALIDATION
# =============================================================================

# Function to check data structure
check_data_structure <- function() {
  cat("Checking data structure...\n")
  
  if (!validate_data_file()) {
    return(NULL)
  }
  
  data <- load_and_prepare_data(analysis_settings$data_source)
  
  cat("Data structure check:\n")
  cat("- Rows:", nrow(data), "\n")
  cat("- Columns:", ncol(data), "\n")
  cat("- Column names:", paste(colnames(data), collapse = ", "), "\n")
  
  # Check for required columns
  required_cols <- c("campaign_name", "spend", "revenue", "impressions", "clicks")
  missing_cols <- setdiff(required_cols, colnames(data))
  
  if (length(missing_cols) > 0) {
    cat("WARNING: Missing recommended columns:", paste(missing_cols, collapse = ", "), "\n")
  } else {
    cat("✓ All recommended columns present\n")
  }
  
  return(data)
}

# Uncomment to check data structure:
# check_data_structure()

# =============================================================================
# EXPORT FUNCTIONS FOR EXTERNAL USE
# =============================================================================

# Export main analysis function
export_analysis <- list(
  run_analysis = run_full_analysis,
  quick_check = quick_analysis,
  get_summary = get_performance_summary,
  analyze_issue = analyze_specific_issue
)

cat("\nAnalysis functions exported and ready for use!\n")
cat("=============================================\n")
# Run complete analysis
run_full_analysis()

# Or run with specific question
run_full_analysis("Analyze CTR performance")
run_full_analysis("Why is ROAS dropping?")

# Quick analysis
quick_analysis()

# Get basic summary
get_performance_summary()    
