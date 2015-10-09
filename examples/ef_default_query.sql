SELECT DISTINCT 
	[GroupBy1].[A1] AS [C1], 
	[GroupBy1].[A2] AS [C2], 
	[Extent1].[trade_num] AS [trade_num], 
	[Extent1].[order_num] AS [order_num], 
	[Extent1].[item_num] AS [item_num], 
	[Extent1].[p_s_ind] AS [p_s_ind], 
	[Extent1].[cmdty_code] AS [cmdty_code], 
	[Extent1].[contr_qty] AS [contr_qty], 
	[Extent2].[credit_term_code] AS [credit_term_code], 
	[Extent2].[pay_term_code] AS [pay_term_code], 
	[Extent2].[del_term_code] AS [del_term_code], 
	[Extent3].[contr_date] AS [contr_date], 
	[Extent8].[mot_short_name] AS [mot_short_name], 
	1 AS [C3], 
	CASE WHEN ([Extent14].[entity_tag_key] IS NULL) THEN N'''' ELSE [Extent14].[target_key1] END AS [C4], 
	CASE WHEN (([Extent4].[alloc_num] IS NULL) AND ([Extent4].[alloc_item_num] IS NULL)) THEN CAST(NULL AS int) ELSE [Extent4].[alloc_num] END AS [C5], 
	CASE WHEN (([Extent4].[alloc_num] IS NULL) AND ([Extent4].[alloc_item_num] IS NULL)) THEN CAST(NULL AS int) ELSE  CAST( [Extent4].[alloc_item_num] AS int) END AS [C6], 
	CASE WHEN ([Extent6].[cost_num] IS NOT NULL) THEN [Extent6].[cost_amt] ELSE [Extent7].[cost_amt] END AS [C7], 
	CASE WHEN (([Extent4].[alloc_num] IS NULL) AND ([Extent4].[alloc_item_num] IS NULL)) THEN [Extent2].[del_date_from] ELSE  CAST( [Extent4].[nomin_date_from] AS datetime) END AS [C8], 
	CASE WHEN (([Extent4].[alloc_num] IS NULL) AND ([Extent4].[alloc_item_num] IS NULL)) THEN [Extent2].[del_date_to] ELSE  CAST( [Extent4].[nomin_date_to] AS datetime) END AS [C9], 
	CASE WHEN (([Extent12].[alloc_num] IS NULL) AND ([Extent12].[alloc_item_num] IS NULL) AND ([Extent12].[ai_est_actual_num] IS NULL)) THEN CAST(NULL AS datetime) WHEN (([Extent13].[alloc_num] IS NULL) AND ([Extent13].[alloc_item_num] IS NULL) AND ([Extent13].[ai_est_actual_num] IS NULL)) THEN CAST(NULL AS datetime) ELSE  CAST( [Extent13].[ai_est_actual_date] AS datetime) END AS [C10], 
	CASE WHEN (([Extent12].[alloc_num] IS NULL) AND ([Extent12].[alloc_item_num] IS NULL) AND ([Extent12].[ai_est_actual_num] IS NULL)) THEN CAST(NULL AS datetime) ELSE  CAST( [Extent12].[ai_est_actual_date] AS datetime) END AS [C11], 
	CASE WHEN (([Extent4].[alloc_num] IS NULL) AND ([Extent4].[alloc_item_num] IS NULL)) THEN CAST(NULL AS datetime) ELSE [Extent4].[estimate_event_date] END AS [C12], 
	CASE WHEN (([Extent20].[formula_num] IS NULL) AND ([Extent20].[price_term_num] IS NULL)) THEN N''Fixed'' ELSE [Extent20].[event_name] END AS [C13], 
	[Extent5].[bl_date] AS [bl_date], 
	[Extent5].[nor_date] AS [nor_date], 
	[Extent9].[acct_short_name] AS [acct_short_name], 
	[Extent15].[target_key1] AS [target_key1], 
	[Extent16].[target_key1] AS [target_key11], 
	[Extent17].[target_key1] AS [target_key12]
	FROM                    [dbo].[trade_item] AS [Extent1]
	INNER JOIN [dbo].[trade_item_wet_phy] AS [Extent2] ON ([Extent1].[trade_num] = [Extent2].[trade_num]) AND ([Extent1].[order_num] = [Extent2].[order_num]) AND ([Extent1].[item_num] = [Extent2].[item_num])
	INNER JOIN [dbo].[trade] AS [Extent3] ON [Extent1].[trade_num] = [Extent3].[trade_num]
	LEFT OUTER JOIN [dbo].[allocation_item] AS [Extent4] ON ([Extent1].[trade_num] = [Extent4].[trade_num]) AND ([Extent1].[order_num] = [Extent4].[order_num]) AND ([Extent1].[item_num] = [Extent4].[item_num])
	LEFT OUTER JOIN [dbo].[allocation_item_transport] AS [Extent5] ON ([Extent4].[alloc_num] = [Extent5].[alloc_num]) AND ([Extent4].[alloc_item_num] = [Extent5].[alloc_item_num])
	LEFT OUTER JOIN [dbo].[cost] AS [Extent6] ON ([Extent1].[trade_num] = [Extent6].[cost_owner_key6]) AND (( CAST( [Extent1].[order_num] AS int) = [Extent6].[cost_owner_key7]) OR (( CAST( [Extent1].[order_num] AS int) IS NULL) AND ([Extent6].[cost_owner_key7] IS NULL))) AND (( CAST( [Extent1].[item_num] AS int) = [Extent6].[cost_owner_key8]) OR (( CAST( [Extent1].[item_num] AS int) IS NULL) AND ([Extent6].[cost_owner_key8] IS NULL))) AND (([Extent4].[alloc_num] = [Extent6].[cost_owner_key1]) OR (([Extent4].[alloc_num] IS NULL) AND ([Extent6].[cost_owner_key1] IS NULL))) AND (( CAST( [Extent4].[alloc_item_num] AS int) = [Extent6].[cost_owner_key2]) OR (( CAST( [Extent4].[alloc_item_num] AS int) IS NULL) AND ([Extent6].[cost_owner_key2] IS NULL)))
	LEFT OUTER JOIN [dbo].[cost] AS [Extent7] ON ([Extent1].[trade_num] = [Extent7].[cost_owner_key6]) AND (( CAST( [Extent1].[order_num] AS int) = [Extent7].[cost_owner_key7]) OR (( CAST( [Extent1].[order_num] AS int) IS NULL) AND ([Extent7].[cost_owner_key7] IS NULL))) AND (( CAST( [Extent1].[item_num] AS int) = [Extent7].[cost_owner_key8]) OR (( CAST( [Extent1].[item_num] AS int) IS NULL) AND ([Extent7].[cost_owner_key8] IS NULL))) AND ([Extent1].[trade_num] = [Extent7].[cost_owner_key1])
	INNER JOIN [dbo].[mot] AS [Extent8] ON [Extent2].[mot_code] = [Extent8].[mot_code]
	LEFT OUTER JOIN [dbo].[account] AS [Extent9] ON [Extent3].[acct_num] = [Extent9].[acct_num]
	LEFT OUTER JOIN [dbo].[assign_trade] AS [Extent10] ON ([Extent1].[trade_num] = [Extent10].[trade_num]) AND ([Extent1].[order_num] = [Extent10].[order_num]) AND ([Extent1].[item_num] = [Extent10].[item_num])
	LEFT OUTER JOIN [dbo].[lc] AS [Extent11] ON [Extent10].[ct_doc_num] = [Extent11].[lc_num]
	LEFT OUTER JOIN [dbo].[ai_est_actual] AS [Extent12] ON (''A'' = [Extent12].[ai_est_actual_ind]) AND ([Extent4].[alloc_num] = [Extent12].[alloc_num]) AND ([Extent4].[alloc_item_num] = [Extent12].[alloc_item_num])
	LEFT OUTER JOIN [dbo].[ai_est_actual] AS [Extent13] ON (''E'' = [Extent13].[ai_est_actual_ind]) AND ([Extent4].[alloc_num] = [Extent13].[alloc_num]) AND ([Extent4].[alloc_item_num] = [Extent13].[alloc_item_num])
	LEFT OUTER JOIN [dbo].[entity_tag] AS [Extent14] ON (5 = [Extent14].[entity_tag_id]) AND (((LTRIM(RTRIM(STR( CAST( [Extent1].[real_port_num] AS float))))) = [Extent14].[key1]) OR ((LTRIM(RTRIM(STR( CAST( [Extent1].[real_port_num] AS float)))) IS NULL) AND ([Extent14].[key1] IS NULL)))
	LEFT OUTER JOIN [dbo].[entity_tag] AS [Extent15] ON (70 = [Extent15].[entity_tag_id]) AND (((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_num] AS float))))) = [Extent15].[key1]) OR ((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_num] AS float)))) IS NULL) AND ([Extent15].[key1] IS NULL))) AND (((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_item_num] AS float))))) = [Extent15].[key2]) OR ((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_item_num] AS float)))) IS NULL) AND ([Extent15].[key2] IS NULL)))
	LEFT OUTER JOIN [dbo].[entity_tag] AS [Extent16] ON (71 = [Extent16].[entity_tag_id]) AND (((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_num] AS float))))) = [Extent16].[key1]) OR ((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_num] AS float)))) IS NULL) AND ([Extent16].[key1] IS NULL))) AND (((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_item_num] AS float))))) = [Extent16].[key2]) OR ((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_item_num] AS float)))) IS NULL) AND ([Extent16].[key2] IS NULL)))
	LEFT OUTER JOIN [dbo].[entity_tag] AS [Extent17] ON (72 = [Extent17].[entity_tag_id]) AND (((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_num] AS float))))) = [Extent17].[key1]) OR ((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_num] AS float)))) IS NULL) AND ([Extent17].[key1] IS NULL))) AND (((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_item_num] AS float))))) = [Extent17].[key2]) OR ((LTRIM(RTRIM(STR( CAST( [Extent4].[alloc_item_num] AS float)))) IS NULL) AND ([Extent17].[key2] IS NULL)))
	INNER JOIN  (SELECT 
		[Extent18].[trade_num] AS [K1], 
		[Extent18].[order_num] AS [K2], 
		[Extent18].[item_num] AS [K3], 
		MAX([Extent18].[nominal_start_date]) AS [A1], 
		MAX([Extent18].[nominal_start_date]) AS [A2]
		FROM [dbo].[quote_pricing_period] AS [Extent18]
		GROUP BY [Extent18].[trade_num], [Extent18].[order_num], [Extent18].[item_num] ) AS [GroupBy1] ON ([Extent1].[trade_num] = [GroupBy1].[K1]) AND ([Extent1].[order_num] = [GroupBy1].[K2]) AND ([Extent1].[item_num] = [GroupBy1].[K3])
	LEFT OUTER JOIN [dbo].[trade_formula] AS [Extent19] ON ([Extent1].[trade_num] = [Extent19].[trade_num]) AND ([Extent1].[order_num] = [Extent19].[order_num]) AND ([Extent1].[item_num] = [Extent19].[item_num])
	LEFT OUTER JOIN [dbo].[event_price_term] AS [Extent20] ON [Extent19].[formula_num] = [Extent20].[formula_num]
	WHERE (([Extent11].[lc_num] IS NULL) AND ( NOT ([Extent9].[acct_short_name] LIKE N''%DUMMY%'')) AND ([Extent2].[credit_term_code] IN (''LC'',''DOCLC'',''SBLC'',''LCCNFM'')) AND ([Extent3].[contr_date] >= @p__linq__0) AND ([Extent3].[contr_date] <= @p__linq__1) AND ((( CAST( [Extent12].[ai_est_actual_date] AS datetime) >= @p__linq__2) AND ( CAST( [Extent12].[ai_est_actual_date] AS datetime) <= @p__linq__3)) OR (( CAST( [Extent13].[ai_est_actual_date] AS datetime) >= @p__linq__4) AND ( CAST( [Extent13].[ai_est_actual_date] AS datetime) <= @p__linq__5)) OR ([Extent13].[ai_est_actual_date] IS NULL)) AND (''WPP'' = [Extent6].[cost_type_code]) AND (''CLOSED'' <> [Extent6].[cost_status])) OR (([Extent11].[lc_num] IS NULL) AND ( NOT ([Extent9].[acct_short_name] LIKE N''%DUMMY%'')) AND ([Extent2].[credit_term_code] IN (''LC'',''DOCLC'',''SBLC'',''LCCNFM'')) AND ([Extent3].[contr_date] >= @p__linq__6) AND ([Extent3].[contr_date] <= @p__linq__7) AND (''WPP'' = [Extent7].[cost_type_code]) AND (''CLOSED'' <> [Extent7].[cost_status]))