import sys

def initialize():
    '''
    read the file 'records.txt' or prompt for initial amount of money
    '''
    initial_money = 0
    records = []
    try: # (7) make sure the file exist
        with open('records.txt') as fh:
            line = fh.readline()
            if line == '': #(8) make sure there is line in the file
                raise ValueError
            try: #(9) make sure the first line can be transformed into integer
                initial_money = int(line)
            except ValueError:
                raise ValueError
            for lines in fh.readlines():
                parts = lines.strip().split() #split the line and remove the space
                if len(parts) != 3: #(10) make sure other lines can be split into a list
                    raise ValueError  #raise an error   
                date, desc, amt = parts
                try: #(10) make sure second string can be transformed into integer
                    amt = int(amt)
                except ValueError:
                    raise ValueError 
                records.append((date,desc,amt)) # add those line read in into record
            print('Welcome back!')
            return initial_money, records
    except (FileNotFoundError,ValueError):
        sys.stderr.write(f'Invalid format in records.txt. Deleting the contents.')
        while True: 
            try:  #(1) make sure money can be transformed into integer
                initial_money = int(input('\nHow much money do you have? '))
                break
            except ValueError: 
                sys.stderr.write(f'Invalid value for money. Set to 0 by default\n')
                break
        return initial_money, records

def add(initial_money,records):
    '''
    prompt for some records and add them into the record list 
    '''
    date = input('When did you receive or spend(yyyy/mm/dd): ')  # prompt user the date of record
    date_check= date.split('/')
    if len(date_check) != 3: # make sure date includes year, month and day
        sys.stderr.write(f'Invalid format. It should be yyyy/mm/dd.\n')
        return initial_money,records
    year,month,day = date_check
    try: # make sure that date can be transfromed into integer for the check later
        year = int(year)
        month = int(month)
        day = int(day)
    except ValueError:
        sys.stderr.write('Invalid format. Date should input number only.\n')
        return initial_money,records
    if month < 1 or month > 12: # make sure month is correct
        sys.stderr.write('Invalid format. Month should between 01 and 12.\n')
        return initial_money,records
    if day < 1 or day > 31: # make sure day is correct
        sys.stderr.write('Invalid format. Day should between 01 and 31.\n')
        return initial_money,records
    items = input('Add some expense or income records with description and amount:\n\r\
desc1 amt1, desc2 amt2, desc3 amt3,...\n')
    for item in items.split(','):
        try: #(3) make sure the user inputs a string that follow the format
            desc, amt = item.split() 
            try: #(4) make sure the second string can be transformed into integer
                desc = desc.strip() 
                amt = int(amt) # transform into integer
                records.append((date,desc,amt))  # add into a list
                initial_money += amt  # caculate balance
            except ValueError:
                sys.stderr.write(f'Invalid value for money.\n')
                sys.stderr.write(f'Fail to add a record.\n')
        except ValueError:
            sys.stderr.write(f'The format of a record should be like this: breakfast -50.\n')
            sys.stderr.write(f'Fail to add a record.\n') 
    return initial_money,records

def view(initial_money,records):
    '''
    print the records
    '''
    steps = []  # create a step list and append each view step
    steps.append(f'Here"s your expense and income records:')
    steps.append(f"{'Date':<10}{'Description':^33}{'Amount':>7}") # align to the right format
    dash_line = '=' * 10 + ' ' +'='*30 + ' ' + '=' * 10
    steps.append(dash_line)
    for date,desc,amt in records: # take out each item
        steps.append(f'{date:<10}{desc:^33}{amt:>7}')
    steps.append(dash_line)
    steps.append(f'Now you have {initial_money} dollars.')
    print('\n'.join(steps)) # print each step in line
    return None

def delete(initial_money,records):
    '''
    prompt for a record to delete and delete it from the record list
    '''
    try: #(5) make sure the user inputs a string that follow the format 
        date, item = input('Which record do you want to delete? \n\
date(yyyy/mm/dd), desc amt\n').strip().split(',')
        date_check= date.split('/')
        if len(date_check) != 3: # make sure date includes year, month and day
            sys.stderr.write(f'Invalid format. It should be yyyy/mm/dd.\n')
            return initial_money,records
        year,month,day = date_check
        try: # make sure that date can be transfromed into integer for the check later
            year = int(year)
            month = int(month)
            day = int(day)
        except ValueError:
            sys.stderr.write('Invalid format. Date should input number only.\n')
            return initial_money,records
        if month < 1 or month > 12: # make sure month is correct
            sys.stderr.write('Invalid format. Month should between 01 and 12.\n')
            return initial_money,records
        if day < 1 or day > 31: # make sure day is correct
            sys.stderr.write('Invalid format. Day should between 01 and 31.\n')
            return initial_money,records
        desc,amt = item.strip().split()
        desc = desc.strip()
        amt = int(amt)
        found = False # (6) make sure the specified record exist
        for i, (d,description,price) in enumerate(records):
             # make sure the desc, amt and position is correct
            if description == desc and price == amt and d  == date: 
                initial_money -= amt  # caculate balance
                records.pop(i) # pop out the item
                found = True
                break
        if not found:
            print(f"There's no record with {date} {desc} {amt}. Fail to delete a record.")
    except ValueError:
        sys.stderr.write(f'Invalid format.')
        sys.stderr.write(f'Fail to delete a record.\n')
    return initial_money,records

def save(initial_money,records):
    ''' 
    write the records to the file 'records.txt' 
    ''' 
    with open('records.txt','w') as fh: 
        fh.write(f'{initial_money}\n') # write the initial money first 
        records_write = [f'{date} {desc} {amt}' for date, desc, amt in records] # make item save like the format of (description price) 
        records_write = '\n'.join(records_write) # make each item records save in line 
        fh.writelines(f'{records_write}') 
    return None

initial_money, records = initialize()
while True:
    command = input('\nWhat do you want to do (add/ view/ delete/ exit)? ')
    # (2) make sure the user input a valid command
    if command == 'add':
        initial_money,records = add(initial_money,records)
    elif command == 'view':
        view(initial_money,records)
    elif command == 'delete':
        initial_money,records = delete(initial_money,records)
    elif command == 'exit':
        save(initial_money,records)
        break
    else:
        sys.stderr.write(f'Invalid command. Try again.\n')
    
        
    