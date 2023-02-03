local upload = require('resty.upload')
local cjson = require('cjson')

local chunk_size = 4096
local form ,err = upload:new(chunk_size)
if not form then
    ngx.log(ngx.ERR, "failed to new upload: ", err)
    ngx.exit(ngx.HTTP_INTERNAL_SERVER_ERROR)
end

form:set_timeout(5 * 60 * 1000)
local conf = {max_size=1000 * 1000000, allow_exts={'txt', 'threaddump', 'log', 'phd', 'hprof', 'zip', 'jar'}}

local my_my_doc_root = ngx.var.my_doc_root
local function get_upload_home(dRoot)
	if dRoot then
		return dRoot..'/dumpfiles'
	else
		return '/srv/dumpfiles'
	end
end
local upload_home = get_upload_home(my_my_doc_root)

local file
local file_name

local current_k
local current_v
local kv_part = {}

local function get_ext(res)
    local filename = ngx.re.match(res,'(.+)filename="(.+)\\.(.+)"(.*)')
    return filename[3]
end

local function check_upload_type(res)
    local ext
    if res == 'text/plain' then
        ext = 'txt'
    elseif res == 'application/java-archive' then
        ext = 'jar'
    elseif res == 'application/zip' then
        ext = 'zip'
    elseif res == 'application/octet-stream' then
        ext = 'phd'
    end
    return ext
end

local function in_array(v, tab)
    local i = false
    for _, val in ipairs(tab) do
        if val == v then
            i = true
            break
        end
    end
    return i
end

local function get_filename(res)
    local filename = ngx.re.match(res,'(.+)filename="(.+)"(.*)')
    if filename then
        return filename[2]
    end
end

local function get_kv_name(res)
    local kvname = ngx.re.match(res,'(.+)name="(.+)"(.*)')
    if kvname then
        return kvname[2]
    end
end

while true do
    local typ, res, err = form:read()
    if not typ then
        ngx.say(cjson.encode({code=502, msg='failed to read:', data=err}))
        return
    end
    if typ == "header" then
        if res[1] == "Content-Disposition" then
            local upload_file_name = get_filename(res[2])

	    if upload_file_name then  -- multipart the file part
		local extension = get_ext(res[2])
            	if not extension then
                 	ngx.say(cjson.encode({code=501, msg='ext not found, it must be txt, threaddump, log, jar, zip, hprof or phd.', data=res}))
                 	return
            	end

            	if not in_array(extension, conf.allow_exts) then
                	ngx.say(cjson.encode({code=501, msg='upload file type not supported, it must be txt, threaddump, log, jar, zip, hprof or phd.', data=res}))
                	return
            	end

            	local dir = upload_home..'/'..os.date('%Y%m%d')..'/'
            	local status = os.execute('mkdir -p '..dir)
            
		if not status then
                	ngx.say(cjson.encode({code=501, msg='mkdir for the upload home failed'}))
                	return
            	end
            	file_name = dir..upload_file_name
            	if file_name then
                	file = io.open(file_name, "w+")
                	if not file then
                    		ngx.say(cjson.encode({code=500, msg='failed to create file for the upload file',data=res}))
                    		return
                	end
            	end
	    else  -- multipart non-file part
		current_k = get_kv_name(res[2])
	    end
	elseif res[1] == "Content-Type" then
            local upload_file_type = check_upload_type(res[2])
            if not upload_file_type then
                 ngx.say(cjson.encode({code=501, msg='cloud not determine the upload file type. It must be txt, threaddump, log, jar, zip, hprof or phd.', data=res}))
                 return
            end
            if not in_array(upload_file_type, conf.allow_exts) then
           	ngx.say(cjson.encode({code=501, msg='upload file type not supported with current content-type, it must be txt, threadump, log, jar, zip, hprof or phd.', data=res}))
           	return
            end
        end
     elseif typ == "body" then
        if type(tonumber(res)) == 'number' and tonumber(res) > conf.max_size then
            ngx.say(cjson.encode({code=501, msg='oversize', data=res}))
            return
        end
        if file then
            file:write(res)
	else
	    current_v = res
	    kv_part[current_k] = current_v
        end
    elseif typ == "part_end" then
        if file then
            file:close()
            file = nil
        end
    elseif typ == "eof" then
	-- upload finished, now call postgrest to insert a record into db to kickstart a job to analyze the upload file.
	if not file_name then
	    ngx.status = ngx.HTTP_BAD_REQUEST
	    ngx.say("No file uploaded, if you're using curl command, you should make sure put a @ symbol before the file name.")
        else
            local file_uri = string.gsub(file_name, my_my_doc_root, '')
	    local download_url = ngx.var.scheme.."://"..ngx.var.http_host..file_uri
	    local job_create_payload = {tag=kv_part["parsetype"], contents={tag='HttpUrl', contents=download_url}}
	    local job_create_req = {status='queued', payload=job_create_payload}
	    ngx.log(ngx.INFO, cjson.encode(job_create_req))
	    ngx.req.set_header("Content-Type", "application/json")
	    ngx.req.set_header("Accept", "application/json")
	    ngx.req.set_header("Prefer", "return=headers-only")
            local job_create_res = ngx.location.capture("/rest/rpc/submit_parsedump_job", {method = ngx.HTTP_POST, body = cjson.encode(job_create_req)})
	    if job_create_res then
                ngx.status = job_create_res.status
	        ngx.header["Content-Type"] = job_create_res.header["Content-Type"]
                -- local id_location = ngx.re.match(job_create_res.header["Location"], '(.+)=eq\\.(.+)')
	        -- local theId
	        -- if id_location then
                --    theId = id_location[2]
	        -- end
	        -- ngx.header = job_create_res.header
                -- ngx.say(cjson.encode({ id = theId }))
                ngx.say(job_create_res.body)
	    end
	end
        break
    else

    end
end
