-record(messagerl_cli_contact, {name,
								token}).
-record(messagerl_cli_groups, {name,
							  members=[]}).
-record(messagerl_cli_msg, {time,
							from,
							group = none,
							read = none,
							content}).
