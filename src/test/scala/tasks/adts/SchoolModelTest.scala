package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import u03.Optionals.*
import Optional.*
import u03.Sequences.*
import Sequence.*

class SchoolModelTest:

  val schoolModel: SchoolModel = BasicSchoolModel
  import schoolModel.*

//  val school: School = emptySchool
  val teacherName: String = "Mario"
  val courseName: String = "PPS"
  val emptyTeacher: Teacher = createTeacher("", Nil())
  val emptyCourse: Course = createCourse("")

  @Test def addTeacher(): Unit =
    assertEquals(createSchool(Cons(createTeacher(teacherName, Nil()), Nil()), Nil()), emptySchool.addTeacher(teacherName))

  @Test def addCourse(): Unit =
    assertEquals(createSchool(Nil(), Cons(createCourse(courseName), Nil())), emptySchool.addCourse(courseName))

  @Test def getExistingTeacherByName(): Unit =
    assertEquals(Just(createTeacher(teacherName, Nil())), emptySchool.addTeacher(teacherName).teacherByName(teacherName))

  @Test def getNonexistingTeacherByName(): Unit =
    assertEquals(Empty(), emptySchool.addTeacher(teacherName).teacherByName("undefined"))

  @Test def getTeacherByNameWithMultipleTeachers(): Unit =
    assertEquals(Just(createTeacher(teacherName, Nil())), emptySchool
      .addTeacher("Teacher1")
      .addTeacher(teacherName)
      .addTeacher("Teacher2")
      .teacherByName(teacherName))

  @Test def getExistingCourse(): Unit =
    assertEquals(Just(createCourse(courseName)), emptySchool.addCourse(courseName).courseByName(courseName))

  @Test def getNonexistingCourse(): Unit =
    assertEquals(Empty(), emptySchool.addCourse(courseName).courseByName("undefined"))

  @Test def getCourseWithMultipleCourses(): Unit =
    assertEquals(Just(createCourse(courseName)), emptySchool
      .addCourse("Course1")
      .addCourse(courseName)
      .addCourse("Course2")
      .courseByName(courseName)
    )

  @Test def nameOfTeacher(): Unit =
    assertEquals(teacherName, emptySchool.nameOfTeacher(createTeacher(teacherName, Nil())))

  @Test def setTecherToACourse(): Unit =
    val school: School = emptySchool.addTeacher(teacherName).addCourse(courseName)
    val t: Teacher = orElse(school.teacherByName(teacherName), createTeacher("", Nil()))
    val c: Course = orElse(school.courseByName(courseName), createCourse(""))

    assertEquals(createSchool(
      Cons(createTeacher(teacherName, Cons(createCourse(courseName), Nil())), Nil()),
      Cons(c, Nil())), school.setTeacherToCourse(t, c))

  @Test def setTeacherToMultipleCourses(): Unit =
    val courseName1: String = courseName
    val courseName2: String = "Course2"

    val school: School = emptySchool.addTeacher(teacherName).addCourse(courseName).addCourse(courseName2)

    val t: Teacher = orElse(school.teacherByName(teacherName), createTeacher("", Nil()))
    val c1: Course = orElse(school.courseByName(courseName1), createCourse(""))
    val c2: Course = orElse(school.courseByName(courseName2), createCourse(""))

    assertEquals(createSchool(
      Cons(createTeacher(teacherName, Cons(createCourse(courseName2), Cons(createCourse(courseName1), Nil()))), Nil()),
      Cons(createCourse(courseName2), Cons(createCourse(courseName1), Nil()))
    ), school.setTeacherToCourse(t, c1).setTeacherToCourse(t, c2))

  @Test def setMultipleTeachersToACourse() =
    val school: School = emptySchool.addTeacher(teacherName).addTeacher("Teacher1").addCourse(courseName)

    val t1: Teacher = orElse(school.teacherByName(teacherName), emptyTeacher)
    val t2: Teacher = orElse(school.teacherByName("Teacher1"), emptyTeacher)
    val c: Course = orElse(school.courseByName(courseName), emptyCourse)

    assertEquals(
      createSchool(
        Cons(createTeacher("Teacher1", Cons(createCourse(courseName), Nil())), Cons(createTeacher(teacherName, Cons(createCourse(courseName), Nil())), Nil())),
        Cons(createCourse(courseName), Nil())),
      school.setTeacherToCourse(t1, c).setTeacherToCourse(t2, c))


