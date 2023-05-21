all:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log log_dir  *.pod_dir;
	rm -rf adder* dbetcd*
	rm -rf _build test_ebin ebin;
	rm -rf Mnesia.*;
	rm -rf *.dir;
	rm -f rebar.lock;
	rm -rf logs;
	rm -rf cmn_service;
	rm -rf sd_service;
	rm -rf log_service;
#	tests 
	mkdir test_ebin;
	erlc -I include -I ../api_repo -o test_ebin test/*.erl;
	rm -rf test_ebin;
#  	dependencies
	mkdir ebin;
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
build:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir;
	rm -rf deployments *_info_specs;
	rm -rf _build test_ebin ebin;
	rm -rf logs;
	rm -rf cmn_service;
	rm -rf sd_service;
	rm -rf log_service;
	rm -f  rebar.lock;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;


clean:
	rm -rf  *~ */*~ src/*.beam test/*.beam
	rm -rf erl_cra*;
	rm -rf spec.*;
	rm -rf test_ebin
	rm -rf ebin;
	rm -rf Mnesia.*;
	rm -rf *.dir;
	rm -rf logs;
	rm -rf cmn_service;
	rm -rf sd_service;
	rm -rf log_service;


eunit:
	rm -rf  *~ */*~ src/*.beam tests/*.beam
	rm -rf erl_cra*;	
	rm -rf test_ebin
	rm -rf ebin;
	rm -rf logs;
	rm -rf Mnesia.*;
	rm -rf *.dir;
	rm -f rebar.lock;
#	tests 
	mkdir test_ebin;
	cp  test/*.app test_ebin;
	erlc -I include -I ../api_repo  -o test_ebin test/*.erl;
#  	dependencies
	rm -rf cmn_service;
	git clone https://github.com/joq62/cmn_service.git;
	erlc -I include -I ../api_repo -o test_ebin cmn_service/src/*.erl
	rm -rf sd_service;
	git clone https://github.com/joq62/sd_service.git;
	erlc -I include -I ../api_repo -o test_ebin sd_service/src/*.erl;
	rm -rf log_service;
	git clone https://github.com/joq62/log_service.git;
	erlc -I include -I ../api_repo -o test_ebin log_service/src/*.erl;
#	dbetcd_appl
	rm -rf dbetcd_appl;	
	git clone https://github.com/joq62/dbetcd_appl;
	cp dbetcd_appl/ebin/* test_ebin;
#	Applications
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	erl -pa ebin -pa test_ebin\
	    -config test/test_sys.config\
	    -sname do_test\
	    -run $(m) start\
	    -setcookie a_cookie
