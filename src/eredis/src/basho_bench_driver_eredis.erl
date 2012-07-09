-module(basho_bench_driver_eredis).

-export([new/1,
         run/4]).

-export([value_gen/1]).

new(_Id) ->
    case whereis(eredis_driver) of
        undefined ->
            case eredis:start_link() of
                {ok, Client} ->
                    register(eredis_driver, Client),
                    {ok, Client};
                {error, Reason} ->
                    {error, Reason}
            end;
        Pid ->
            {ok, Pid}
    end.

run(get, KeyGen, _ValueGen, Client) ->
    Start = KeyGen(),
    %%case eredis:q(["MGET" | lists:seq(Start, Start + 500)]) of
    case catch(eredis:q(Client, ["GET", Start], 100)) of
        {ok, _Value} ->
            {ok, Client};
        {error, Reason} ->
            {error, Reason, Client};
        {'EXIT', {timeout, _}} ->
            {error, timeout, Client}
    end;

run(pipeline_get, KeyGen, _ValueGen, Client) ->
    Seq = lists:seq(1, 5),
    P = [["GET", KeyGen()] || _ <- Seq],

    case catch(eredis:qp(Client, P, 500)) of
        {error, Reason} ->
            {error, Reason, Client};
        {'EXIT', {timeout, _}} ->
            {error, timeout, Client};
        Res ->
            case check_pipeline_get(Res, Seq) of
                ok ->
                    {ok, Client};
                {error, Reason} ->
                    {error, Reason, Client}
            end
    end;

run(put, KeyGen, ValueGen, Client) ->
    case catch(eredis:q(Client, ["SET", KeyGen(), ValueGen()], 100)) of
        {ok, <<"OK">>} ->
            {ok, Client};
        {error, Reason} ->
            {error, Reason, Client};
        {'EXIT', {timeout, _}} ->
            {error, timeout, Client}
    end;

run(pipeline_put, KeyGen, ValueGen, Client) ->
    Seq = lists:seq(1, 5),
    P = [["SET", KeyGen(), ValueGen()] || _ <- Seq],
    R = [{ok, <<"OK">>} || _ <- Seq],

    case catch(eredis:qp(Client, P, 500)) of
        R ->
            {ok, Client};
        {error, Reason} ->
            {error, Reason, Client};
        {'EXIT', {timeout, _}} ->
            {error, timeout, Client}
    end.


check_pipeline_get([], []) ->
    ok;
check_pipeline_get([{ok, _}|Res], [_|Seq]) ->
    check_pipeline_get(Res, Seq);
check_pipeline_get([{error, Reason}], _) ->
    {error, Reason}.


value_gen(_Id) ->
    fun() ->
            %% %% Example data from http://json.org/example.html
            <<"{\"web-app\":{\"servlet\":[{\"servlet-name\":\"cofaxCDS\",\"servlet-class\":\"org.cofax.cds.CDSServlet\",\"init-param\":{\"configGlossary:installationAt\":\"Philadelphia,PA\",\"configGlossary:adminEmail\":\"ksm@pobox.com\",\"configGlossary:poweredBy\":\"Cofax\",\"configGlossary:poweredByIcon\":\"/images/cofax.gif\",\"configGlossary:staticPath\":\"/content/static\",\"templateProcessorClass\":\"org.cofax.WysiwygTemplate\",\"templateLoaderClass\":\"org.cofax.FilesTemplateLoader\",\"templatePath\":\"templates\",\"templateOverridePath\":\"\",\"defaultListTemplate\":\"listTemplate.htm\",\"defaultFileTemplate\":\"articleTemplate.htm\",\"useJSP\":false,\"jspListTemplate\":\"listTemplate.jsp\",\"jspFileTemplate\":\"articleTemplate.jsp\",\"cachePackageTagsTrack\":200,\"cachePackageTagsStore\":200,\"cachePackageTagsRefresh\":60,\"cacheTemplatesTrack\":100,\"cacheTemplatesStore\":50,\"cacheTemplatesRefresh\":15,\"cachePagesTrack\":200,\"cachePagesStore\":100,\"cachePagesRefresh\":10,\"cachePagesDirtyRead\":10,\"searchEngineListTemplate\":\"forSearchEnginesList.htm\",\"searchEngineFileTemplate\":\"forSearchEngines.htm\",\"searchEngineRobotsDb\":\"WEB-INF/robots.db\",\"useDataStore\":true,\"dataStoreClass\":\"org.cofax.SqlDataStore\",\"redirectionClass\":\"org.cofax.SqlRedirection\",\"dataStoreName\":\"cofax\",\"dataStoreDriver\":\"com.microsoft.jdbc.sqlserver.SQLServerDriver\",\"dataStoreUrl\":\"jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon\",\"dataStoreUser\":\"sa\",\"dataStorePassword\":\"dataStoreTestQuery\",\"dataStoreTestQuery\":\"SETNOCOUNTON;selecttest='test';\",\"dataStoreLogFile\":\"/usr/local/tomcat/logs/datastore.log\",\"dataStoreInitConns\":10,\"dataStoreMaxConns\":100,\"dataStoreConnUsageLimit\":100,\"dataStoreLogLevel\":\"debug\",\"maxUrlLength\":500}},{\"servlet-name\":\"cofaxEmail\",\"servlet-class\":\"org.cofax.cds.EmailServlet\",\"init-param\":{\"mailHost\":\"mail1\",\"mailHostOverride\":\"mail2\"}},{\"servlet-name\":\"cofaxAdmin\",\"servlet-class\":\"org.cofax.cds.AdminServlet\"},{\"servlet-name\":\"fileServlet\",\"servlet-class\":\"org.cofax.cds.FileServlet\"},{\"servlet-name\":\"cofaxTools\",\"servlet-class\":\"org.cofax.cms.CofaxToolsServlet\",\"init-param\":{\"templatePath\":\"toolstemplates/\",\"log\":1,\"logLocation\":\"/usr/local/tomcat/logs/CofaxTools.log\",\"logMaxSize\":\"\",\"dataLog\":1,\"dataLogLocation\":\"/usr/local/tomcat/logs/dataLog.log\",\"dataLogMaxSize\":\"\",\"removePageCache\":\"/content/admin/remove?cache=pages&id=\",\"removeTemplateCache\":\"/content/admin/remove?cache=templates&id=\",\"fileTransferFolder\":\"/usr/local/tomcat/webapps/content/fileTransferFolder\",\"lookInContext\":1,\"adminGroupID\":4,\"betaServer\":true}}],\"servlet-mapping\":{\"cofaxCDS\":\"/\",\"cofaxEmail\":\"/cofaxutil/aemail/*\",\"cofaxAdmin\":\"/admin/*\",\"fileServlet\":\"/static/*\",\"cofaxTools\":\"/tools/*\"},\"taglib\":{\"taglib-uri\":\"cofax.tld\",\"taglib-location\":\"/WEB-INF/tlds/cofax.tld\"}}">>
    end.
