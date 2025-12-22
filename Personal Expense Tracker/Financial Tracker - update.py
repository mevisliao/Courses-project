import  sys 
  
class Record:
    '''
    represent a record
    '''
    def __init__(self,date,cat,desc,amt):  # instantiated
        self._date = date
        self._cat = cat
        self._desc = desc
        self._amt = amt
    
    # getter method
    @property
    def date(self):
        return self._date
    @property
    def category(self):
        return self._cat
    @property
    def description(self):
        return self._desc
    @property
    def amount(self):
        return self._amt


class  Records:
    '''
    Maintain a list of all the 'Record's and the initial amount of money.
    ''' 
    def __init__(self):
        self._initial_money = 0
        self._records = []
        try: # (7) make sure the file exist
            with open('records.txt') as fh:
                line = fh.readline()
                if line == '': #(8) make sure there is line in the file
                    raise ValueError
                try: #(9) make sure the first line can be transformed into integer
                    self._initial_money = int(line)
                except ValueError:
                    raise ValueError
                for lines in fh.readlines():
                    parts = lines.strip().split() #split the line and remove the space
                    if len(parts) != 4: #(10) make sure other lines can be split into a list
                        raise ValueError  #raise an error   
                    date, cat, desc, amt = parts
                    try: #(10) make sure second string can be transformed into integer
                        amt = int(amt)
                    except ValueError:
                        raise ValueError 
                    self._records.append(Record(date,cat,desc,amt)) # add those line read in into record
                print('Welcome back!')
        except (FileNotFoundError,ValueError):
            sys.stderr.write(f'Invalid format in records.txt. Deleting the contents.')
            while True: 
                try:  #(1) make sure money can be transformed into integer
                    self._initial_money = int(input('\nHow much money do you have? '))
                    break
                except ValueError: 
                    sys.stderr.write(f'Invalid value for money. Set to 0 by default\n')
                    break
    
    def add(self, record,categories): 
        '''
        prompt for some records and add them into the record list 
        '''
        for item in record.split(','):
            parts = item.split()
            if len(parts) != 4: #(3) make sure the user inputs a string that follow the format
                sys.stderr.write(f'Invalid format. It should be like this: 2025/11/17 meal breakfast -50.\n.')
                return 
            date,cat,desc,amt = parts
            try:
                year,month,day = map(int,date.split('/'))  # make sure it can be transformed into integer
            except ValueError:
                sys.stderr.write('Invalid value. Date should be integer.')
                return 
            try:
                dt = DateTime(year,month,day)  # call DateTime 
            except ValueError:
                return
            dt = repr(dt)
            cat = ''.join(cat.split())
            if categories.is_category_valid(cat) == False:
                print('The specified category is not in the category list.')
                print('You can check the category list by command "view categories". ')
                print('Fail to add a record.')
                return 
            desc = desc.strip()
            try: #(4) make sure the second string can be transformed into integer
                amt = int(amt) # transform into integer
            except ValueError:
                sys.stderr.write(f'Invalid value for money.\n')
                sys.stderr.write(f'Fail to add a record.\n')
                return
            self._records.append(Record(dt,cat,desc,amt))  # add into a list
            self._initial_money += amt  # caculate balance
            return self._initial_money,self._records
    
    def view(self): 
        '''
        print the records
        '''
        steps = []  # create a step list and append each view step
        steps.append(f'Here"s your expense and income records:')
        steps.append(f"{'Date':<12}{'Category':^16}{'Description':^22}{'Amount':>10}") # align to the right format
        dash_line = '=' * 60
        steps.append(dash_line)
        for rec in self._records: # take out each item 
            steps.append(f'{rec.date:<12}{rec.category:^16}{rec.description:^22}{rec.amount:>10}')
        steps.append(dash_line)
        steps.append(f'Now you have {self._initial_money} dollars.')
        print('\n'.join(steps)) # print each step in line
        return None
    
    def delete(self,delete_record): 
        '''
        prompt for a record to delete and delete it from the record list
        '''
        parts = delete_record.split()
        if len(parts) != 4: #(5) make sure the user inputs a string that follow the format
            sys.stderr.write(f'Invalid format.')
            sys.stderr.write(f'Fail to delete a record.\n')
            return 
        date,cat,desc,amt = parts
        try:
            year,month,day = map(int,date.split('/'))
        except ValueError:
            sys.stderr.write('Invalid value. Date should be integer.')  # make sure it can be transformed into integer
            return 
        try:
            dt = DateTime(year,month,day)  # call DateTime
        except ValueError:
            return
        dt = repr(dt)
        cat = cat.strip()
        desc = desc.strip()
        try: #(4) make sure the second string can be transformed into integer
            amt = int(amt) # transform into integer
        except ValueError:
            sys.stderr.write(f'Invalid value for money.\n')
            sys.stderr.write(f'Fail to delete a record.\n')
            return
        found = False # (6) make sure the specified record exist
        for i, rec in enumerate(self._records):
             # make sure the desc, amt and position is correct
            if rec.category == cat and rec.description == desc and rec.amount == amt and rec.date  == dt: 
                self._initial_money -= amt  # caculate balance
                self._records.pop(i) # pop out the item
                found = True
                break
        if not found:
            print(f"There's no record with {dt} {cat} {desc} {amt}. Fail to delete a record.")
        return self._initial_money, self._records
    
    def find(self,target_categories):
        '''
        find the target record and print
        '''
        steps = []
        steps.append(f'Here"s your expense and income records under category "{category}": ')
        steps.append(f"{'Date':<12}{'Category':^16}{'Description':^22}{'Amount':>10}")
        dash_line = '=' * 60
        steps.append(dash_line)
        filtered = list(filter(lambda r: r.category in target_categories, self._records)) # only print the subcategories record
        total_amount = 0
        for rec in filtered: # take out each item
            steps.append(f'{rec.date:<12}{rec.category:^16}{rec.description:^22}{rec.amount:>10}')
            total_amount += rec.amount  # calculate balance
        steps.append(dash_line)
        print('\n'.join(steps))
        print(f'The total amount above is {total_amount}')
        return None
    
    def save(self): 
        ''' 
        write the records to the file 'records.txt' 
        ''' 
        with open('records.txt','w') as fh: 
            fh.write(f'{self._initial_money}\n') # write the initial money first 
            records_write = [f'{rec.date} {rec.category} {rec.description} {rec.amount}' for rec in self._records] # make item save like the format of (description price) 
            records_write = '\n'.join(records_write) # make each item records save in line 
            fh.writelines(f'{records_write}') 


class Categories: 
    """
    Maintain the category list and provide some  methods.
    """
    def __init__(self): 
        '''
        a multi-level list of categories
        '''
        self._categories = ['expense', ['food', ['meal', 'snack', 'drink'], 'transportation', 
 ['bus', 'railway']  ], 'income', ['salary', 'bonus']]   
    
    def view(self): 
        '''
        print the categories
        '''
        def print_categories(categories,level=0):
            if categories == None:
                return
            if isinstance(categories,list):
                for child in categories:
                    print_categories(child,level+1)  # recursion: record the sublists
            else:
                print(f'{' '*level+'-'} {categories}')
        return print_categories(self._categories,level = 0)
        
    def is_category_valid(self,category):
        '''
        Check if the specified category is in the predefined list categories 
        '''
        def check(cat,categories):
            if isinstance(categories,list):
                for child in categories:
                    if check(cat, child): # recursion
                        return True    
                return False
            else:
                return cat == categories
        return check(category,self._categories)
    
    def find_subcategories(self,category): 
        '''
        find all subcategories
        '''
        def find_subcategories_gen(category,categories,found = False):
            '''
            A generator that yields the target category  and its subcategories
            '''
            if isinstance(categories,list):
                for index,child in enumerate(categories):
                    yield from find_subcategories_gen(category,child,found)  # yield the target category
                    if child == category and index +1 < len(categories) and \
                        type(categories[index+1]) == list:   
                        yield from find_subcategories_gen(category,categories[index+1],True)  #yield the subcategories
            else: 
                if category == categories or found == True:    # if not a list or find the target
                    yield categories
        return list(find_subcategories_gen(category, self._categories,found = False))


class DateTime:
    '''
    make sure the date is valid
    '''
    def __init__(self,year,month,day):
        self._year = year
        self.is_month_valid(month)
        self._month = month
        self.is_day_valid(month, day)
        self._day = day
    
    @staticmethod
    def leap(year):
        return (year%400 == 0) or ((year % 4) == 0) and (year % 100 != 0)
    
    def is_month_valid(self,month):
        '''
        check month is valid
        '''
        if month < 1 or month > 12:
            sys.stderr.write('Invalid format. Month should between 01 and 12.\n')
            raise ValueError
        return True
    
    def is_day_valid(self,month,day):
        '''
        check day is valid
        '''
        if month in {1,3,5,7,8,10,12}:
            if day < 1 or day > 31: 
                sys.stderr.write('Invalid format. Day should between 01 and 31.\n')
                raise ValueError
        elif month in {4,6,9,11}:
            if day < 1 or day > 30:
                sys.stderr.write('Invalid format. Day should between 01 and 30.\n')
                raise ValueError
        elif self.leap(self._year):
            if day < 1 or day > 29:
                sys.stderr.write('Invalid format. Day should between 01 and 29.\n')
                raise ValueError
        else:
            if day < 1 or day > 28:
                sys.stderr.write('Invalid format. Day should between 01 and 28.\n')
                raise ValueError
        return True
    
    def __repr__(self):
        return f'{self._year}/{self._month}/{self._day}'  # print as the format: yyyy/mm/dd
     
     
categories = Categories() 
records = Records() 
while True: 
    command = input('\nWhat do you want to do (add/ view/ delete/ view categories/ find/ exit)? ') 
    if  command ==  'add'  : 
        record = input('Add some expense or income records with category, description and amount (separate by spaces):\n\r\
date1 cat1 desc1 amt1, date2 cat2 desc2 amt2, date3 cat3 desc3 amt3,...\n')
        records.add(record, categories) 
    elif command == 'view': 
        records.view() 
    elif command == 'delete' : 
        delete_record = input("Which record do you want to delete? " ) 
        records.delete(delete_record) 
    elif  command == 'view categories': 
            categories.view() 
    elif  command == 'find': 
        category = input('Which category do you want to find? ') 
        target_categories = categories.find_subcategories(category) 
        records.find(target_categories) 
    elif  command == 'exit': 
        records.save() 
        break 
    else : 
        sys.stderr.write('Invalid command. Try again.\n')
        
