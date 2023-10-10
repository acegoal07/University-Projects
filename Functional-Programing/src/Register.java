import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

public class Register {
   // Hash map used to store students
   private ArrayList<Student> studentRegister = new ArrayList<>();
   // Register constructor
   public Register() {
      // Creates register 
   }
   // Used to add students to the register
   public void addStudents(Student ... students) {
      Collections.addAll(studentRegister, students);
   }
   // Remove student
   public void removeStudentByID(int idNumberInput) {
      studentRegister.removeIf(s -> s.returnIdNumber().equals(idNumberInput));
   }
   // Remove multiple students using
   public void bulkRemoveStudentsByID(int ... idNumberInput) {
      Arrays.stream(idNumberInput).forEach(this::removeStudentByID);
   }
   // Used to return a copy of the register
   public List<Student> returnRegister() {
      return new ArrayList<>(studentRegister);
   }
   // Used to retrieve a specific student using id
   public Student searchStudentByID (int idNumberInput) {
      Optional<Student> returnStudent = studentRegister.stream().filter(s -> s.returnIdNumber().equals(idNumberInput)).limit(1).findFirst();
      if (returnStudent.isPresent()) {
         return returnStudent.get();
      }
      return null;
   }
   // Used to retrieve students that are a certain age
   public List<Student> searchStudentByAge (int ageInput, int limit) {
      if (limit != 0) {
         return studentRegister.stream().filter(s -> s.returnAge().equals(ageInput)).limit(limit).toList();
      } else {
         return studentRegister.stream().filter(s -> s.returnAge().equals(ageInput)).toList();
      }
   }
   // Create a function to get all students on a  specific course
   public List<Student> searchStudentByCourse (String courseInput, int limit) {
      if (limit != 0) {
         return studentRegister.stream().filter(s -> s.returnCourse().equals(courseInput)).limit(limit).toList();
      } else {
         return studentRegister.stream().filter(s -> s.returnCourse().equals(courseInput)).toList();
      }
   }
   // Create a function to get all students on a specific module
   public List<Student> searchStudentByModule (String moduleCodeInput, int limit) {
      if (limit != 0) {
         return studentRegister.stream().filter(s -> Arrays.asList(s.returnModulesCodes()).contains(moduleCodeInput)).limit(limit).toList();
      } else {
         return studentRegister.stream().filter(s -> Arrays.asList(s.returnModulesCodes()).contains(moduleCodeInput)).toList();
      }
   }
}