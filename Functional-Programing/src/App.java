import lib.Module;
import lib.Student;
import lib.Module.ModuleCode;
import lib.Student.CourseType;

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
                CourseType.CS,
                new Module[]{
                        new Module("Test", ModuleCode.CI535)
                }
            ),
            new Student(
                "joe",
                "bog",
                20,
                19,
                1,
                2004,
                8493,
                CourseType.MA,
                new Module[]{
                        new Module("Test", ModuleCode.CI101)
                }
            ),
            new Student(
                "Steve",
                "joe",
                27,
                10,
                4,
                1995,
                7483,
                CourseType.EN,
                new Module[]{}
            )
        );
    }
}