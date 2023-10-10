public class App {
   public static void main(String[] args) {
      // Creates a new register for the students
      Register register = new Register();
      // Adds students to the register
      register.addStudents(
         new Student(
            "Alex",
            "Wood",
            18,
            18,
            1,
            2004,
            1654,
            "Computer Science",
            new Module[]{
               new Module("Test", "test")
            }
         ),
         new Student(
            "joe",
            "bog",
            18,
            10,
            6,
            2005,
            8493,
            "Computer Science",
            new Module[]{
               new Module("Test", "test")
            }
         )
      );
      // Prints out the register
      System.out.println(register.returnRegister());
      // Prints out student using search
      Student student = register.searchStudentByID(1654);
      if (student != null) {
         System.out.println(student.returnDetails());
      } else {
         System.out.println("Student not found");
      }
      register.searchStudentByAge(18, 0).forEach(i -> System.out.println(i.returnBirthday()));
      register.searchStudentByCourse("Computer Science", 0).forEach(i -> System.out.println(i.returnName()));
      register.searchStudentByModule("test", 0).forEach(i -> System.out.println(i.returnIdNumber()));
      // Removes student
      // register.removeStudentByID(1654);
      // register.bulkRemoveStudentsByID(1654, 8493);
      // Prints out the register
      // System.out.println(register.returnRegister());
   }
}