def read_schematics(file_path)
  locks = []
  keys = []
  current_schematic = []
  is_lock = true

  File.readlines(file_path).each do |line|
    line = line.strip
    if line.empty?
      if is_lock
        locks << current_schematic
      else
        keys << current_schematic
      end
      current_schematic = []
      is_lock = !is_lock
    else
      current_schematic << line
    end
  end

  if current_schematic.any?
    if is_lock
      locks << current_schematic
    else
      keys << current_schematic
    end
  end

  [locks, keys]
end

def convert_to_heights(schematic)
  heights = []
  (0...schematic[0].length).each do |col|
    height = 0
    (0...schematic.length).each do |row|
      height += 1 if schematic[row][col] == '#'
    end
    heights << height
  end
  heights
end

def can_fit?(lock_heights, key_heights)
  lock_heights.zip(key_heights).all? { |lh, kh| lh + kh <= 5 }
end

def count_fitting_pairs(locks, keys)
  lock_heights = locks.map { |lock| convert_to_heights(lock) }
  key_heights = keys.map { |key| convert_to_heights(key) }

  count = 0
  lock_heights.each do |lh|
    key_heights.each do |kh|
      count += 1 if can_fit?(lh, kh)
    end
  end
  count
end

if __FILE__ == $0
  locks, keys = read_schematics('December25/input.txt')
  result = count_fitting_pairs(locks, keys)
  puts result
end