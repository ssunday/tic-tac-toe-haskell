echo "Initializing databases..."

createdb ttt_hk

createdb ttt_hk_test

echo "Initializing schema..."

psql -U postgres -d ttt_hk -c 'CREATE TABLE IF NOT EXISTS scores(key SERIAL PRIMARY KEY, winning_player text);'

psql -U postgres -d ttt_hk_test -c 'CREATE TABLE IF NOT EXISTS scores(key SERIAL PRIMARY KEY, winning_player text);'

echo "All done!"
