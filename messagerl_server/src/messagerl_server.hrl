-record(messagerl_contacts, {token,
							 contacts=[]}).
-record(messagerl_profiles, {token,
							 psk,
							 perms=user}).
-record(messagerl_usernames, {token,
							  username_hash}).
-record(messagerl_node, {token,
						 node}).

