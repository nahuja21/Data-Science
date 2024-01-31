import csv
import os
import random
from datetime import datetime

def initialize_csv(filename):
    if not os.path.exists(filename) or os.path.getsize(filename) == 0:
        with open(filename, mode='w', newline='') as file:
            writer = csv.writer(file)
            writer.writerow(["Date (MM-DD-YYYY)", "Category", "Amount", "Notes"])

def add_expense(filename):
    date = input("Enter the date (MM-DD-YYYY): ")
    try:
        datetime.strptime(date, "%m-%d-%Y")
    except ValueError:
        print("Invalid date format. Please try again.")
        return

    category = input("Enter the category of the expense: ")
    amount = input("Enter the amount (numbers only): ")
    notes = input("Enter any notes (optional): ")

    try:
        amount = float(amount)
    except ValueError:
        print("Invalid amount. Please enter a number.")
        return

    with open(filename, mode='a', newline='') as file:
        writer = csv.writer(file)
        writer.writerow([date, category, amount, notes])
        print("Expense added successfully.")

def view_expenses(filename):
    try:
        with open(filename, mode='r') as file:
            reader = csv.reader(file)
            next(reader)  # Skip the header row
            for row in reader:
                print(f"Date: {row[0]}, Category: {row[1]}, Amount: {row[2]}, Notes: {row[3]}")
    except FileNotFoundError:
        print("No data file found. Please add an expense first.")

def clear_data(filename):
    confirmation = input("Are you sure you want to clear all data? This cannot be undone. Enter 'yes' to confirm: ")
    if confirmation.lower() == 'yes':
        with open(filename, mode='w', newline='') as file:
            writer = csv.writer(file)
            writer.writerow(["Date (MM-DD-YYYY)", "Category", "Amount", "Notes"])
            print("Data cleared. File reset with header.")
    else:
        print("Data clear canceled.")

def remove_line(filename):
    date_to_remove = input("Enter the date of the expense to remove (MM-DD-YYYY): ")
    category_to_remove = input("Enter the category of the expense to remove: ")

    lines = []
    with open(filename, 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            lines.append(row)

    with open(filename, 'w', newline='') as file:
        writer = csv.writer(file)
        for row in lines:
            if row[0] != date_to_remove or row[1] != category_to_remove:
                writer.writerow(row)

    print("Expense removed if it existed.")

def edit_line(filename):
    date_to_edit = input("Enter the date of the expense to edit (MM-DD-YYYY): ")
    category_to_edit = input("Enter the category of the expense to edit: ")

    lines = []
    with open(filename, 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            lines.append(row)

    with open(filename, 'w', newline='') as file:
        writer = csv.writer(file)
        for row in lines:
            if row[0] == date_to_edit and row[1] == category_to_edit:
                print("Editing the following expense: ")
                print(f"Date: {row[0]}, Category: {row[1]}, Amount: {row[2]}, Notes: {row[3]}")
                row[0] = input("Enter the new date (MM-DD-YYYY): ")
                row[1] = input("Enter the new category: ")
                row[2] = input("Enter the new amount (numbers only): ")
                row[3] = input("Enter any new notes (optional): ")
            writer.writerow(row)

    print("Expense edited if it existed.")
def create_sample_month_data(filename, num_samples=100):
    categories = ["Food", "Groceries", "Fun", "School", "Work", "Travel", "Misc"]
    year = "2024"
    month = f"{random.randint(1, 12):02d}"

    with open(filename, mode='a', newline='') as file:
        writer = csv.writer(file)
        for _ in range(num_samples):
            day = f"{random.randint(1, 31):02d}"
            date = f"{year}-{month}-{day}"
            category = random.choice(categories)
            amount = round(random.uniform(1, 1000), 2)
            notes = "Sample data"
            writer.writerow([date, category, amount, notes])
def create_yearly_sample_data(filename, year='2024'):
    categories = ["Food", "Groceries", "Fun", "School", "Work", "Travel", "Misc"]
    num_samples_per_month = 50

    with open(filename, mode='a', newline='') as file:
        writer = csv.writer(file)
        for month in range(1, 13):
            for _ in range(num_samples_per_month):
                day = f"{random.randint(1, 31):02d}"
                date = f"{year}-{month:02d}-{day}"
                category = random.choice(categories)
                amount = round(random.uniform(1, 1000), 2)
                notes = "Yearly sample data"
                writer.writerow([date, category, amount, notes])

def main():
    filename = 'expenses.csv'
    initialize_csv(filename)

    while True:
        choice = input("Enter 1 to add an expense, 2 to view expenses, 3 to clear data, 4 to remove a line, 5 to edit a line, 6 to create sample month data, 7 to create yearly sample data, or 8 to quit: ")
        if choice == '1':
            add_expense(filename)
        elif choice == '2':
            view_expenses(filename)
        elif choice == '3':
            clear_data(filename)
        elif choice == '4':
            remove_line(filename)
        elif choice == '5':
            edit_line(filename)
        elif choice == '6':
            create_sample_month_data(filename)
            print("Sample month data created.")
        elif choice == '7':
            create_yearly_sample_data(filename)
            print("Yearly sample data created.")
        elif choice == '8':
            break
        else:
            print("Invalid option. Please try again.")

if __name__ == "__main__":
    main()
