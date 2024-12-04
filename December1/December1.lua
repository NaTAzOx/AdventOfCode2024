function File_exists(file)
    local f = io.open(file, "rb")
    if f then f:close() end
    return f ~= nil
end

function Lines_from(file)
    if not File_exists(file) then return {} end
    local lines = {}
    for line in io.lines(file) do
        lines[#lines+1] = line
    end
    return lines
end

function Split(input, separator)
    if separator == nil then
        separator = "%s"
    end
    local splitted = {}
    for str in string.gmatch(input, "([^" ..separator.. "]+)") do
        table.insert(splitted, str)
    end
    return splitted
end

function CountOccurrence(number, table)
    local counter = 0
    for _, line in pairs(table) do
        if number == line then counter = counter + 1 end
    end
    return counter
end

local file = 'input.txt'
local lines = Lines_from(file)
local count = 0
local distance = 0
local firstNumbers = {}
local secondNumbers = {}

for _, line in pairs(lines) do
    local splitted = Split(line)
    table.insert(firstNumbers, splitted[1])
    table.insert(secondNumbers, splitted[2])
    count = count + 1
end

table.sort(firstNumbers)
table.sort(secondNumbers)

for i = 1,count,1 do
    local distanceBetweenTwo = firstNumbers[i] -  secondNumbers[i]
    distance = distance + (math.abs(distanceBetweenTwo))
end

print(distance)
distance = 0

for i = 1,count,1 do
    local distanceBetweenTwo = firstNumbers[i] * CountOccurrence(firstNumbers[i], secondNumbers)
    distance = distance + (math.abs(distanceBetweenTwo))
end

print(distance)