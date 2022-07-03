package info.kgeorgiy.ja.skroba.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class StudentDB implements StudentQuery {
    private final Comparator<Student> COMPARATOR_OF_STUDENTS_BY_NAME = Comparator
            .comparing(Student::getLastName)
            .thenComparing(Student::getFirstName).reversed()
            .thenComparing(Student::getId);

    private <I, T, C extends Collection<T>> C mapThenCollectExtended(Collection<I> input,
                                                                     Function<I, T> map,
                                                                     Collector<T, ?, C> collector) {
        return input.stream().map(map).collect(collector);
    }

    private <T, C extends Collection<T>> C sortThenCollectExtended(Collection<T> input,
                                                                   Comparator<T> comparator,
                                                                   Collector<T, ?, C> collector) {
        return input.stream()
                .sorted(comparator).collect(collector);
    }

    private <T, C> C filterSortThenCollect(Collection<T> input, Predicate<T> predicate,
                                           Comparator<T> comparator, Collector<T, ?, C> collector) {
        return input.stream().filter(predicate)
                .sorted(comparator).collect(collector);
    }

    private <T, I> Predicate<T> predicateMaker(I comparable, Function<T, I> fun) {
        // :NOTE: no need for deepEquals here
        return input -> Objects.equals(comparable, fun.apply(input));
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return mapThenCollectExtended(students, Student::getFirstName,
                Collectors.toUnmodifiableList());
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return mapThenCollectExtended(students, Student::getLastName,
                Collectors.toUnmodifiableList());
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return mapThenCollectExtended(students, Student::getGroup,
                Collectors.toUnmodifiableList());
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return mapThenCollectExtended(students, s -> s.getFirstName() + " " + s.getLastName(),
                Collectors.toUnmodifiableList());
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return mapThenCollectExtended(students, Student::getFirstName,
                Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream().max(Comparator.naturalOrder())
                .map(Student::getFirstName).orElse("");
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortThenCollectExtended(students, Comparator.naturalOrder(),
                Collectors.toUnmodifiableList());
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortThenCollectExtended(students, COMPARATOR_OF_STUDENTS_BY_NAME,
                Collectors.toUnmodifiableList());
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return filterSortThenCollect(students, predicateMaker(name, Student::getFirstName),
                COMPARATOR_OF_STUDENTS_BY_NAME, Collectors.toUnmodifiableList());
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return filterSortThenCollect(students, predicateMaker(name, Student::getLastName),
                COMPARATOR_OF_STUDENTS_BY_NAME, Collectors.toUnmodifiableList());
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return filterSortThenCollect(students, predicateMaker(group, Student::getGroup),
                COMPARATOR_OF_STUDENTS_BY_NAME, Collectors.toUnmodifiableList());
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return filterSortThenCollect(students, predicateMaker(group, Student::getGroup),
                COMPARATOR_OF_STUDENTS_BY_NAME,
                Collectors.toMap(Student::getLastName, Student::getFirstName, BinaryOperator.minBy(String::compareTo)));
    }
}
