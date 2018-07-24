class Student(object):
    def __init__(self, name, gpa, age):
        self.name = name
        self.gpa = gpa
        self.age = age

    def __str__(self):
        return 'Person: ' + self.name + "GPA: " + self.gpa + "Age: " + self.age
    __repr__ = __str__

    def __lt__(self, other):
        return (self.gpa, self.name, self.age) < (other.gpa, other.name, other.age)

    def __eq__(self, other):
        return (self.gpa, self.name, self.age) == (other.gpa, other.name, other.age)

    def __hash__(self):
        return hash((self.name, self.gpa, self.age))


student_list = [Student("Amy", "3.0", "18"), Student("Bob", "3.3", "20"), Student("Smith", "3.3", "21"),
                Student("Cathy", "3.2", "19"), Student("Mike", "3.8", "16")]
sort_list = sorted(student_list)

# This code belongs to Difu Wu
print(dict(Student("Amy", "3.0", "18").__dict__))

# This idea comes from Henry Liu
student_list = sorted(student_list, key=lambda x: x.age)

print(sort_list)
print(student_list)









