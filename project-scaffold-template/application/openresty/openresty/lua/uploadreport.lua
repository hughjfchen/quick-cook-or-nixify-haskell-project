local upload = require('resty.upload')
local cjson = require('cjson')

local chunk_size = 4096
local form ,err = upload:new(chunk_size)
if not form then
    ngx.log(ngx.ERR, "failed to new upload: ", err)
    ngx.exit(ngx.HTTP_INTERNAL_SERVER_ERROR)
end

form:set_timeout(5 * 60 * 1000)
local conf = {max_size=1000 * 1000000, allow_exts={'htm', 'html', 'tgz'}}

local my_my_doc_root = ngx.var.my_doc_root
local function get_upload_home(dRoot)
	if dRoot then
		return dRoot..'/parsereports'
	else
		return '/srv/parsereports'
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
    if res == 'text/html' then
        ext = 'html'
    elseif res == 'application/zip' then
        ext = 'zip'
    elseif res == 'application/octet-stream' then
        ext = 'tgz'
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
                 	ngx.say(cjson.encode({code=501, msg='ext not found, it must be txt, jar, zip or phd.', data=res}))
                 	return
            	end

            	if not in_array(extension, conf.allow_exts) then
                	ngx.say(cjson.encode({code=501, msg='upload file type not supported, it must be htm, html or zip.', data=res}))
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
                 ngx.say(cjson.encode({code=501, msg='cloud not determine the upload file type. It must be htm, html or zip.', data=res}))
                 return
            end
            if not in_array(upload_file_type, conf.allow_exts) then
           	ngx.say(cjson.encode({code=501, msg='upload file type not supported with current content-type, it must be htm, html or zip.', data=res}))
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
	-- upload finished, now call postgrest to insert a record into db to save the url for the uploaded report.
	local report_path = file_name
	-- if the upload report is zip, we need to unzip it
	local file_ext = string.sub(file_name, -4, -1)
	if file_ext == ".tgz" then
	   local file_without_ext = string.sub(file_name, 1, string.len(file_name)-4)
	   local mk_report_dir_status = os.execute('mkdir -p '..file_without_ext)
	   if not mk_report_dir_status then
              ngx.say(cjson.encode({code=501, msg='mkdir for the upload zip report failed'}))
              return
           end
	   local unzip_report_status = os.execute('cd '..file_without_ext..' ; tar zxf '..file_name)
	   if not unzip_report_status then
              ngx.say(cjson.encode({code=501, msg='untar the upload tgz report failed'}))
              return
           end    
	   report_path=file_without_ext..'/index.html'
	end
        local file_uri = string.gsub(report_path, my_my_doc_root, '')
	local download_url = ngx.var.scheme.."://"..ngx.var.http_host..file_uri
	local report_create_req = { job_id = kv_part["job_id"]
	                          -- , created_at = kv_part["created_at"]
				  -- , updated_at = kv_part["updated_at"] -- use now() instead the old update time.
				  -- , run_at = kv_part["run_at"]
				  -- , status = kv_part["status"]
				  -- , payload = cjson.decode(kv_part["payload"])
				  , last_update = { report_url = download_url }
				  -- , attempts = kv_part["attempts"]
				  -- , locked_at = kv_part["locked_at"]
				  -- , locked_by = kv_part["locked_by"]
				  }
	ngx.log(ngx.INFO, cjson.encode(report_create_req))
	ngx.req.set_header("Content-Type", "application/json")
	ngx.req.set_header("Accept", "application/json")
	-- ngx.req.set_header("Prefer", "return=headers-only")
        local report_create_res = ngx.location.capture("/rest/rpc/update_parse_report", {method = ngx.HTTP_POST, body = cjson.encode(report_create_req)})
	if report_create_res then
            ngx.status = report_create_res.status
	    ngx.header["Content-Type"] = report_create_res.header["Content-Type"]
            -- local id_location = ngx.re.match(report_create_res.header["Location"], '(.+)=eq\\.(.+)')
	    -- local theId
	    -- if id_location then
            --    theId = id_location[2]
	    -- end
	    -- ngx.header = job_create_res.header
            -- ngx.say(cjson.encode({ id = theId }))
            ngx.say(report_create_res.body)
	end
        break
    else

    end
end
