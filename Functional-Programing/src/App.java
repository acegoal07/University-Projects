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
                    new Module(
                        "Functional Programming", 
                        ModuleCode.CI535
                    )
                }
            ),
            new Student(
                "John",
                "Doe",
                1,
                1,
                1,
                2000,
                1234,
                CourseType.CS,
                new Module[]{
                    new Module(
                        "Functional Programming", 
                        ModuleCode.CI535
                    )
                }
            ),
            new Student(
                "Jane",
                "Doe",
                2,
                2,
                2,
                2000,
                1235,
                CourseType.CS,
                new Module[]{
                    new Module(
                        "Functional Programming", 
                        ModuleCode.CI535
                    )
                }
            ),
            new Student(
                "Bob",
                "Smith",
                3,
                3,
                3,
                2000,
                1236,
                CourseType.CS,
                new Module[]{
                    new Module(
                        "Functional Programming", 
                        ModuleCode.CI535
                    )
                }
            ),
            new Student(
                "Alice",
                "Smith",
                4,
                4,
                4,
                2000,
                1237,
                CourseType.CS,
                new Module[]{
                    new Module(
                        "Functional Programming", 
                        ModuleCode.CI535
                    )
                }
            ),
            new Student(
                "Eve",
                "Smith",
                5,
                5,
                5,
                2000,
                1238,
                CourseType.CS,
                new Module[]{
                    new Module(
                        "Functional Programming", 
                        ModuleCode.CI535
                    )
                }
            )
        );
    }
}