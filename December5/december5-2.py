inputText = open('input.txt', 'r')
firstNumbers = []
secondNumbers = []
correctOrderUpdates = []
orderUpdates = []
result = 0

def generate():
    for line in inputText:
        if len(line.split("|")) != 1:
            splitted = line.split("|")
            firstNumbers.append(int(splitted[0]))
            secondNumbers.append(int(splitted[1]))
        elif len(line.split(",")) != 1:
            updates = []
            for number in line.split(","):
                updates.append(int(number))
            orderUpdates.append(updates)


def check_update(update_element):
    reversedOrder = False
    while True:
        swapped = False
        for i in range(len(firstNumbers)):
            first = firstNumbers[i]
            second = secondNumbers[i]
            first_indices = [index for index, value in enumerate(update_element) if value == first]
            second_indices = [index for index, value in enumerate(update_element) if value == second]

            for first_index in first_indices:
                for second_index in second_indices:
                    if first_index > second_index:
                        update_element[first_index], update_element[second_index] = update_element[second_index], update_element[first_index]
                        swapped = True
                        reversedOrder = True
        if not swapped:
            break

    middleIndex = int(len(update_element) / 2)
    if reversedOrder:
        return update_element[middleIndex]
    else:
        return None

generate()

for update in orderUpdates:
    middle = check_update(update)
    if middle is not None:
        result += middle
    print(middle)

print(result)