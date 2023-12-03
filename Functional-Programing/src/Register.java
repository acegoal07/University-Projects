import java.util.*;

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

/////////// RETURNING ///////////

    /**
     * Used to return a copy of the register
     *
     * @return A copy of the register
     */
    public List<Student> returnRegister() {
        return new ArrayList<>(studentRegister);
    }

    /**
     * Used to check whether a student exists in the register
     *
     * @param idNumberInput The ID number of the student you want to check for
     */
    public Boolean checkIfStudentExists(int idNumberInput) {
        return studentRegister.stream().anyMatch(s -> s.returnIdNumber().equals(idNumberInput));
    }

/////////// SEARCHING ///////////   

    /**
     * Used to search for a student by ID
     *
     * @param idNumberInput The ID number of the student you want to search for
     * @return The student you searched for
     */
    public Student searchStudentByID(int idNumberInput) {
        Optional<Student> returnStudent = studentRegister.stream().filter(s -> s.returnIdNumber().equals(idNumberInput)).limit(1).findFirst();
        return returnStudent.orElse(null);
    }

    /**
     * Used to get all students of a specific age
     *
     * @param ageInput The age you want to search for
     * @param limit    The maximum amount of students you want to return (0 for no limit)
     * @return The students of the age
     */
    public List<Student> searchStudentByAge(int ageInput, int limit) {
        return studentRegister.stream().filter(s -> s.returnAge() == ageInput).limit(limit == 0 ? Long.MAX_VALUE : limit).toList();
    }

    /**
     * Used to get all students on a specific course
     *
     * @param courseInput The course you want to search for
     * @param limit       The maximum amount of students you want to return (0 for no limit)
     * @return The students on the course
     */
    public List<Student> searchStudentByCourse(CourseType courseInput, int limit) {
        return studentRegister.stream().filter(s -> s.returnCourse() == courseInput).limit(limit == 0 ? Long.MAX_VALUE : limit).toList();
    }

    /**
     * Used to get all students on a specific module
     *
     * @param moduleCodeInput The module code of the module you want to search for
     * @param limit           The maximum amount of students you want to return
     * @return The students on the module
     */
    public List<Student> searchStudentByModule(ModuleCode moduleCodeInput, int limit) {
        return studentRegister.stream().filter(s -> Arrays.asList(s.returnModulesCodes()).contains(moduleCodeInput.toString())).limit(limit == 0 ? Long.MAX_VALUE : limit).toList();
    }

/////////// SORTING ///////////

    /**
     * Used to sort the register by full name in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByFullName(Boolean descending, List<Student> studentList) {
        if (studentList != null) {
            return studentList.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnName().compareTo(s1.returnName())) : (Comparator.comparing(Student::returnName))).toList();
        } else {
            return studentRegister.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnName().compareTo(s1.returnName())) : (Comparator.comparing(Student::returnName))).toList();
        }
    }

    /**
     * Used to sort the register by course in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByFirstName(Boolean descending, List<Student> studentList) {
        if (studentList != null) {
            return studentList.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnFirstName().compareTo(s1.returnFirstName())) : (Comparator.comparing(Student::returnFirstName))).toList();
        } else {
            return studentRegister.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnFirstName().compareTo(s1.returnFirstName())) : (Comparator.comparing(Student::returnFirstName))).toList();
        }
    }

    /**
     * Used to sort the register by course in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByLastName(Boolean descending, List<Student> studentList) {
        if (studentList != null) {
            return studentList.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnLastName().compareTo(s1.returnLastName())) : (Comparator.comparing(Student::returnLastName))).toList();
        } else {
            return studentRegister.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnLastName().compareTo(s1.returnLastName())) : (Comparator.comparing(Student::returnLastName))).toList();
        }
    }

    /**
     * Used to sort the register by age in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByAge(Boolean descending, List<Student> studentList) {
        if (studentList != null) {
            return studentList.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnAge().compareTo(s1.returnAge())) : (Comparator.comparing(Student::returnAge))).toList();
        } else {
            return studentRegister.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnAge().compareTo(s1.returnAge())) : (Comparator.comparing(Student::returnAge))).toList();
        }
    }

    /**
     * Used to sort the register by birthday in ascending or descending order
     *
     * @param descending  Whether you want the register to be sorted in descending order
     * @param studentList The list of students you want to sort
     * @return The sorted register
     */
    public List<Student> sortByBirthday(Boolean descending, List<Student> studentList) {
        if (studentList != null) {
            return studentList.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnBirthday().compareTo(s1.returnBirthday())) : (Comparator.comparing(Student::returnBirthday))).toList();
        } else {
            return studentRegister.stream().sorted(Boolean.TRUE.equals(descending) ? ((s1, s2) -> s2.returnBirthday().compareTo(s1.returnBirthday())) : (Comparator.comparing(Student::returnBirthday))).toList();
        }
    }
}