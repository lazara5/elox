Zoo = {}
Zoo.__index = Zoo

function Zoo.create()
	local zoo = {}
	setmetatable(zoo, Zoo)
	zoo.aarvark  = 1
    zoo.baboon   = 1
    zoo.cat      = 1
    zoo.donkey   = 1
    zoo.elephant = 1
    zoo.fox      = 1
	return zoo
end

function Zoo:ant() return self.aarvark end
function Zoo:banana() return self.baboon end
function Zoo:tuna() return self.cat end
function Zoo:hay() return self.donkey end
function Zoo:grass() return self.elephant end
function Zoo:mouse() return self.fox end

local zoo = Zoo:create()
local sum = 0
local start = os.clock()
local batch = 0
while (os.clock() - start < 10) do
	for i = 0, 9999 do
		sum = sum + zoo:ant()
			+ zoo:banana()
            + zoo:tuna()
            + zoo:hay()
            + zoo:grass()
            + zoo:mouse()
	end
	batch = batch + 1
end
print(sum)
print(batch)
print(os.clock() - start)