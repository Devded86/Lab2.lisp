<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">Студент: Марчук Дмитро Андрійович група КВ-22<p>
<p align="right">Рік: 2025<p>
	
## Загальне завдання	
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом (п. 2.1.1). Вимоги до функцій:
1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (див. п. 2.3).

## Варіант 15
1. Написати функцію spread-values , яка заміняє nil в списку на попередній не-
nil елемент:
  ```lisp
CL-USER> (spread-values '(nil 1 2 nil 3 nil nil 4 5))
(NIL 1 2 2 3 3 3 4 5)
```
2. Написати функцію delete-duplicates , яка видаляє всі послідовні дублікати тих
елементів з вхідного списку атомів, послідовних дублікатів яких більше за задане
число:
```lisp
CL-USER> (delete-duplicates '(1 1 2 3 3 3 2 2 a a a b) 3)
(1 1 2 3 2 2 A B)
```

## Лістинг функції spread-values
```lisp
(defun spread-values (list &optional previous-value)
  "Заміняє nil в списку на попередній не-nil елемент.
   list - вхідний список.
   previous-value - акумулятор (за замовчуванням nil)."
  (if (null list) 
      nil
      (let ((current (car list))
            (rest (cdr list)))
        (if (null current)
            ;; Якщо поточний nil, підставляємо попередній і йдемо далі
            (cons previous-value (spread-values rest previous-value))
            ;; Якщо поточний не nil, він стає новим попереднім
            (cons current (spread-values rest current))))))
```

### Тестові набори
```lisp
(defun check-spread-values (name input expected)
  "Execute spread-values on input, compare result with expected."
  (let ((result (spread-values input))) 
    (format t "~:[~a failed!~%  Expected: ~a~%  Obtained: ~a~;~a passed!~%  Expected: ~a~%  Obtained: ~a~]~%"
            (equal result expected)
            name expected result)))

(defun test-spread-values ()
  (format t "~%Start testing spread-values function~%")
  (check-spread-values "test 1" 
                       '(nil 10 20 nil 30 nil nil 40 50) 
                       '(nil 10 20 20 30 30 30 40 50))
  (check-spread-values "test 2" 
                       '(nil nil 5 5 nil 8 nil nil nil) 
                       '(nil nil 5 5 5 8 8 8 8))
  (check-spread-values "test 3" 
                       '(100 nil 0 nil 5) 
                       '(100 100 0 0 5))
  (format t "End spread-values tests~%~%"))
```

### Тестування
```lisp
CL-USER>  (test-spread-values)
Start testing spread-values function
test 1 passed!
  Expected: (NIL 10 20 20 30 30 30 40 50)
  Obtained: (NIL 10 20 20 30 30 30 40 50)
test 2 passed!
  Expected: (NIL NIL 5 5 5 8 8 8 8)
  Obtained: (NIL NIL 5 5 5 8 8 8 8)
test 3 passed!
  Expected: (100 100 0 0 5)
  Obtained: (100 100 0 0 5)
End spread-values tests
```

## Лістинг функції delete-duplicates-sequence
```lisp
((defun delete-duplicates-sequence (lst n)
  "Видаляє послідовні дублікати, якщо їх кількість >= n, залишаючи один екземпляр."
  (labels 
      ;; --- Локальна функція 1: Підрахунок дублікатів ---
      ((count-duplicates (sub-lst element count)
         (if (and sub-lst (eql element (car sub-lst)))
             (count-duplicates (cdr sub-lst) element (1+ count))
             count))
       
       ;; --- Локальна функція 2: Пропуск дублікатів ---
       (drop-duplicates (sub-lst element)
         (if (and sub-lst (eql element (car sub-lst)))
             (drop-duplicates (cdr sub-lst) element)
             sub-lst)))

    ;; --- Основне тіло функції ---
    (if (null lst) 
        nil
        (let ((first (car lst))          
              (rest (cdr lst)))    
          
          ;; Перевіряємо, чи починається серія дублікатів
          (if (and rest (eql first (car rest)))
              (let ((count (count-duplicates rest first 1))) 
                (if (>= count n) 
                    ;; Серія >= N: залишаємо 1 елемент, решту пропускаємо
                    (cons first (delete-duplicates-sequence (drop-duplicates rest first) n)) 
                    ;; Серія < N: просто йдемо далі по одному елементу
                    (cons first (delete-duplicates-sequence rest n))))
              
              ;; Дублікату немає
              (cons first (delete-duplicates-sequence rest n)))))))
```

### Тестові набори
```lisp
(defun check-delete-duplicates (name input expected quantity)
  (let ((result (delete-duplicates-sequence input quantity)))
    (format t "~:[~a failed!~%  Expected: ~a~%  Obtained: ~a~;~a passed!~%  Expected: ~a~%  Obtained: ~a~]~%"
            (equal result expected) name expected result)))

(defun test-delete-duplicates ()
  (format t "Start testing delete-duplicates-sequence function~%")
  
  ;; Тест 1: Символи. Серія 'X (3 шт) >= 3 -> стискається. 
  (check-delete-duplicates "test 1" 
                           '(10 10 20 30 30 30 20 20 X X X Y Y) 
                           '(10 10 20 30 20 20 X Y Y) 
                           3)
  
  ;; Тест 2: Nil та числа. Поріг N=2. Всі пари (і nil, і 5) стискаються.
  (check-delete-duplicates "test 2" 
                           '(nil nil 5 5 nil 8 nil nil nil) 
                           '(nil 5 nil 8 nil) 
                           2)
  
  ;; Тест 3: Група елементів у самому кінці списку.
  (check-delete-duplicates "test 3" 
                           '(7 5 8 8 8 0 0 4 4 5 5 5) 
                           '(7 5 8 0 0 4 4 5) 
                           3)
                           
  (format t "End delete-duplicates tests~%"))
```

### Тестування
```lisp
CL-USER> (test-delete-duplicates)
Start testing delete-duplicates-sequence function
test 1 passed!
  Expected: (10 10 20 30 20 20 X Y Y)
  Obtained: (10 10 20 30 20 20 X Y Y)
test 2 passed!
  Expected: (NIL 5 NIL 8 NIL)
  Obtained: (NIL 5 NIL 8 NIL)
test 3 passed!
  Expected: (7 5 8 0 0 4 4 5)
  Obtained: (7 5 8 0 0 4 4 5)
End delete-duplicates tests
```
