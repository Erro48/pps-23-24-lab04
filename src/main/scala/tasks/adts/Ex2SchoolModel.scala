package tasks.adts

import tasks.adts.SchoolModel.BasicSchoolModel.{Course, Teacher}
import u03.Sequences.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

  trait SchoolModel:
    type School
    type Teacher
    type Course

    def emptySchool: School
    def createSchool(teachers: Sequence[Teacher], courses: Sequence[Course]): School
    def createTeacher(name: String, courses: Sequence[Course]): Teacher
    def createCourse(name: String): Course

    extension (school: School)
      def addTeacher(name: String): School /* Add teacher to School.teachers list */
      def addCourse(name: String): School /* Add course to School.courses list */
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  case class CourseType(name: String)

  case class TeacherType(name: String, courses: Sequence[CourseType])

  case class SchoolType(teachers: Sequence[TeacherType], courses: Sequence[CourseType])

  object BasicSchoolModel extends SchoolModel:

    import Optional.*
    import Sequence.*

    opaque type School = SchoolType
    opaque type Teacher = TeacherType
    opaque type Course = CourseType

    def emptySchool: School = createSchool(Nil(), Nil())
    def createSchool(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolType(teachers, courses)

    def createTeacher(name: String, courses: Sequence[Course]): Teacher = TeacherType(name, courses)

    def createCourse(name: String): Course = CourseType(name)

    extension (school: School)
      def addTeacher(name: String): School = school match
        case School(teachers, courses) => SchoolType(Cons(createTeacher(name, Nil()), teachers), courses)

      def addCourse(name: String): School = school match
        case School(teachers, courses) => SchoolType(teachers, Cons(createCourse(name), courses))

      def teacherByName(name: String): Optional[Teacher] = school match
        case School(teachers, _) => searchTeacher(teachers)(name)
        case _ => Empty()

      def courseByName(name: String): Optional[Course] = school match
        case School(_, courses) => searchCourse(courses)(name)
        case _ => Empty()

      def nameOfTeacher(teacher: Teacher): String = teacher match
        case TeacherType(n, _) => n

      def nameOfCourse(course: Course): String = course match
        case CourseType(n) => n

      def setTeacherToCourse(teacher: Teacher, course: Course): School =

        val t: Teacher = Sequence.map
        // get teachers with the same name as `teacher`
          (filter(getTeachers())(nameOfTeacher(_) == nameOfTeacher(teacher)))
        // ...to that teachers add `course`
          (_ match {case TeacherType(name, courses) => TeacherType(name, Cons(course, courses))}) match
            case Cons(h, _) => h  // take the first one if present
            case _ => createTeacher(nameOfTeacher(teacher), Cons(course, Nil()))

        val otherTeachers: Sequence[Teacher] = filter(getTeachers())(nameOfTeacher(_) != nameOfTeacher(teacher))

        createSchool(
          Cons(t, otherTeachers),
          Cons(course, filter(getCourses())(_ != course))
        )

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher match
          case TeacherType(_, c) => c


      private def searchTeacher(teachers: Sequence[Teacher])(teacherName: String): Optional[Teacher] = teachers match
        case Cons(h, _) if nameOfTeacher(h) == teacherName => Just(h)
        case Cons(_, tail) => searchTeacher(tail)(teacherName)
        case _ => Empty()

      private def searchCourse(courses: Sequence[Course])(name: String): Optional[Course] = courses match
        case Cons(h, _) if getCourseName(h) == name => Just(h)
        case Cons(_, tail) => searchCourse(tail)(name)
        case _ => Empty()

      private def getCourseName(course: Course): String = course match
        case CourseType(name) => name

      private def getTeachers(): Sequence[Teacher] = school match
        case SchoolType(t, _) => t

      private def getCourses(): Sequence[Course] = school match
        case SchoolType(_, c) => c