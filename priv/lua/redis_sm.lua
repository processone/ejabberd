redis.replicate_commands()
local cursor = redis.call('GET', KEYS[3]) or 0
local scan_result = redis.call('HSCAN', KEYS[1], cursor, 'COUNT', ARGV[1])
local newcursor = scan_result[1]
local cursor = redis.call('SET', KEYS[3], newcursor)
redis.call('EXPIRE', KEYS[3], 30)
for key,value in ipairs(scan_result[2]) do
   local uskey, sidkey = string.match(value, '(.*)||(.*)')
   if uskey and sidkey then
      redis.call('HDEL', uskey, sidkey)
      redis.call('HDEL', KEYS[1], value)
   else
      redis.call('HDEL', KEYS[2], value)
   end
end
return newcursor
