IP=$(dig +short myip.opendns.com @resolver1.opendns.com)
cd ./ebin/
erl -name messagerl_client@$IP
