import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;

import lib.Student;
import lib.Module.ModuleCode;
import lib.Student.CourseType;

public class Register {

    // ArrayList used to store students
    private final ArrayList<Student> studentRegister = new ArrayList<>();

    // Register constructor
    public Register() {
        // Creates register
    }

/////////// ADDING AND REMOVING ///////////

    /**
     * Used to add students to the register
     *
     * @param students The students you want to add to the register
     */
    public void addStudents(Student... students) {
        // check if a student already has the id
        for (Student student : students) {
            if (Boolean.FALSE.equals(checkIfStudentExists(student.returnIdNumber()))) {
                studentRegister.add(student);
            } else {
                System.out.println("Student with ID " + student.returnIdNumber() + " already exists");
            }
        }
    }

    /**
     * Used to remove a student from the register by the ID number
     *
     * @param idNumberInput The ID number of the student you want to remove
     */
    public void removeStudentByID(int idNumberInput) {
        studentRegister.removeIf(s -> s.returnIdNumber().equals(idNumberInput));
    }

    /**
     * Used to remove multiple students from the register by the ID number
     *
     * @param idNumberInput The ID number of the students you want to remove
     */
    public void bulkRemoveStudentsByID(int... idNumberInput) {
        Arrays.stream(idNumberInput).forEach(this::removeStudentByID);
    }

/////////// Checks ///////////

    /**
     * Used to check whether a student exists in the register
     *
     * @param idNumberInput The ID number of the student you want to check for
     * @return Whether the student exists
     */
    public Boolean checkIfStudentExists(int idNumberInput) {
        return studentRegister.stream()
            .anyMatch(s -> s.returnIdNumber()
                .equals(idNumberInput)
            );
    }

/////////// SEARCHING ///////////

    /**
     * Used to get all students on a specific module
     * 
     * @param predicate The predicate you want to use to search for students
     * @param limit The maximum amount of students you want to return (0 for no limit)
     * @return The students on the module
     */
    public List<Student> search(Predicate<Student> predicate, int limit) {
        return studentRegister.stream()
            .filter(predicate)
            .limit(limit == 0 ? Long.MAX_VALUE : limit)
            .toList();
    }

    /**
     * Used to search for a student by ID
     *
     * @param idNumberInput The ID number of the student you want to search for
     * @return The student you searched for
     */
    public Student searchStudentByID(int idNumberInput) {
        return search(s -> s.returnIdNumber() == idNumberInput, 1).get(0);
    }

    /**
     * Used to get all students on a specific module
     *
     * @param moduleCodeInput The module code of the module you want to search for
     * @param limit The maximum amount of students you want to return
     * @return The students on the module
     */
    public List<Student> searchStudentByModule(ModuleCode moduleCodeInput, int limit) {
        return search(s -> Arrays.stream(s.returnModulesCodes())
            .anyMatch(mc -> mc == moduleCodeInput), limit);
    }

    /**
     * Used to get all students of a specific age
     *
     * @param ageInput The age you want to search for
     * @param limit The maximum amount of students you want to return (0 for no limit)
     * @return The students of the age
     */
    public List<Student> searchStudentByAge(int ageInput, int limit) {
        return search(s -> s.returnAge() == ageInput, limit);
    }
    
    /**
     * Used to get all students on a specific course
     *
     * @param courseInput The course you want to search for
     * @param limit The maximum amount of students you want to return (0 for no limit)
     * @return The students on the course
     */
    public List<Student> searchStudentByCourse(CourseType courseInput, int limit) {
        return search(s -> s.returnCourse() == courseInput, limit);
    }

    /**
     * Used to get all students of a specific first name
     *
     * @param firstNameInput The first name you want to search for
     * @param limit The maximum amount of students you want to return (0 for no limit)
     * @return The students of the first name
     */
    public List<Student> searchStudentByFirstName(String firstNameInput, int limit) {
        return search(s -> s.returnFirstName().equals(firstNameInput), limit);
    }

    /**
     * Used to get all students of a specific last name
     *
     * @param lastNameInput The last name you want to search for
     * @param limit The maximum amount of students you want to return (0 for no limit)
     * @return The students of the last name
     */
    public List<Student> searchStudentByLastName(String lastNameInput, int limit) {
        return search(s -> s.returnLastName().equals(lastNameInput), limit);
    }

    /**
     * Used to get all students of a specific full name
     *
     * @param fullNameInput The full name you want to search for
     * @param limit The maximum amount of students you want to return (0 for no limit)
     * @return The students of the full name
     */
    public List<Student> searchStudentByFullName(String fullNameInput, int limit) {
        return search(s -> s.returnName().equals(fullNameInput), limit);
    }

/////////// SORTING ///////////

    /**
     * Used to sort the register by a given comparator
     * @param descending Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @param comparator The comparator you want to use to sort the register
     * @return The sorted register
     */
    public List<Student> sort(boolean descending, List<Student> studentList, Comparator<Student> comparator) {
        return (studentList != null ? studentList : studentRegister).stream()
            .sorted(descending ? comparator.reversed() : comparator)
            .toList();
    }

    /**
     * Used to sort the register by ID in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByID(boolean descending, List<Student> studentList) {
        return sort(descending, studentList, Comparator.comparing(Student::returnIdNumber));
    }

    /**
     * Used to sort the register by full name in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByFullName(boolean descending, List<Student> studentList) {
        return sort(descending, studentList, Comparator.comparing(Student::returnName));
    }

    /**
     * Used to sort the register by course in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByFirstName(boolean descending, List<Student> studentList) {
        return sort(descending, studentList, Comparator.comparing(Student::returnFirstName));
    }

    /**
     * Used to sort the register by course in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByLastName(boolean descending, List<Student> studentList) {
        return sort(descending, studentList, Comparator.comparing(Student::returnLastName));
    }

    /**
     * Used to sort the register by age in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByAge(boolean descending, List<Student> studentList) {
        return sort(descending, studentList, Comparator.comparing(Student::returnAge));
    }

    /**
     * Used to sort the register by birthday in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByBirthday(boolean descending, List<Student> studentList) {
        return sort(descending, studentList, Comparator.comparing(Student::returnBirthday));
    }
}