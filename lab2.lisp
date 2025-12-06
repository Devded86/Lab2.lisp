;;; Завдання 1: spread-values

(defun spread-values (list &optional previous-value)
  "Заміняє nil в списку на попередній не-nil елемент.
   
   list           - вхідний список.
   previous-value - акумулятор, що зберігає останнє знайдене значення 
                    (за замовчуванням nil)."
  
  ;; Базовий випадок: якщо список пустий, повертаємо nil (кінець рекурсії)
  (if (null list) 
      nil
      (let ((current (car list))   ; Поточний елемент (голова)
            (rest (cdr list)))     ; Решта списку (хвіст)
        
        (if (null current)
            ;; Якщо поточний елемент nil:
            ;; 1. В результат записуємо previous-value (замінюємо nil).
            ;; 2. Рекурсивно викликаємо функцію для хвоста, передаючи те ж саме previous-value.
            (cons previous-value (spread-values rest previous-value))
            
            ;; Якщо поточний елемент НЕ nil:
            ;; 1. Залишаємо його як є (cons current ...).
            ;; 2. Рекурсивно викликаємо функцію, але тепер current стає новим previous-value.
            (cons current (spread-values rest current))))))

;;; --- Тестові набори та утиліти для spread-values ---

(defun check-spread-values (name input expected)
  "Запускає spread-values на вхідних даних і порівнює з очікуваним результатом."
  (let ((result (spread-values input))) 
    (format t "~:[~a failed!~%  Expected: ~a~%  Obtained: ~a~;~a passed!~%  Expected: ~a~%  Obtained: ~a~]~%"
            (equal result expected) ; Перевірка рівності
            name expected result)))

(defun test-spread-values ()
  (format t "~%Start testing spread-values function~%")
  
  ;; Тест 1: Звичайні числа. Nil на початку залишається Nil, інші замінюються.
  (check-spread-values "test 1" 
                       '(nil 10 20 nil 30 nil nil 40 50) 
                       '(nil 10 20 20 30 30 30 40 50))
  
  ;; Тест 2: Групи Nil. Перевірка, що Nil замінюється на останнє число перед групою.
  (check-spread-values "test 2" 
                       '(nil nil 5 5 nil 8 nil nil nil) 
                       '(nil nil 5 5 5 8 8 8 8))
  
  ;; Тест 3: Чергування. Перевірка коректної зміни "попереднього" значення.
  (check-spread-values "test 3" 
                       '(100 nil 0 nil 5) 
                       '(100 100 0 0 5))
                       
  (format t "End spread-values tests~%~%"))

;;; Завдання 2: delete-duplicates

(defun delete-duplicates-sequence (lst n)
  "Видаляє послідовні дублікати, якщо їх кількість >= n, залишаючи лише один екземпляр.
   Використовує локальні функції (labels) для допоміжних обчислень."
  
  (labels 
      ;; --- Локальна функція 1: Підрахунок дублікатів ---
      ((count-duplicates (sub-lst element count)
         "Рахує кількість підряд однакових елементів (element) на початку списку."
         (if (and sub-lst (eql element (car sub-lst)))
             (count-duplicates (cdr sub-lst) element (1+ count))
             count))
       
       ;; --- Локальна функція 2: Пропуск дублікатів ---
       (drop-duplicates (sub-lst element)
         "Пропускає (видаляє) всі елементи на початку списку, які дорівнюють element."
         (if (and sub-lst (eql element (car sub-lst)))
             (drop-duplicates (cdr sub-lst) element)
             sub-lst)))

    ;; --- Основне тіло функції ---
    (if (null lst) 
        nil ; Базовий випадок: список пустий
        (let ((first (car lst))          
              (rest (cdr lst)))    
          
          ;; Перевіряємо, чи наступний елемент такий самий, як поточний
          (if (and rest (eql first (car rest)))
              
              ;; ТАК, почалася серія дублікатів. Рахуємо їх кількість.
              (let ((count (count-duplicates rest first 1))) 
                (if (>= count n) 
                    ;; Якщо дублікатів >= n:
                    ;; 1. Залишаємо один екземпляр (cons first ...).
                    ;; 2. Пропускаємо всі інші копії за допомогою drop-duplicates.
                    (cons first (delete-duplicates-sequence (drop-duplicates rest first) n)) 
                    
                    ;; Якщо дублікатів менше ніж n:
                    ;; Нічого не видаляємо, просто йдемо далі по одному елементу.
                    (cons first (delete-duplicates-sequence rest n))))
              
              ;; НІ, дублікату немає (наступний елемент інший або кінець списку).
              (cons first (delete-duplicates-sequence rest n)))))))

;;; --- Тестові набори та утиліти для delete-duplicates ---

(defun check-delete-duplicates (name input expected quantity)
  "Запускає delete-duplicates-sequence і порівнює результат."
  (let ((result (delete-duplicates-sequence input quantity)))
    (format t "~:[~a failed!~%  Expected: ~a~%  Obtained: ~a~;~a passed!~%  Expected: ~a~%  Obtained: ~a~]~%"
            (equal result expected) name expected result)))

(defun test-delete-duplicates ()
  (format t "Start testing delete-duplicates-sequence function~%")
  
  ;; Тест 1: Символи. Серія 'X (3 шт) >= 3 -> стискається. Серія 'Y (2 шт) < 3 -> залишається.
  (check-delete-duplicates "test 1" 
                           '(10 10 20 30 30 30 20 20 X X X Y Y) 
                           '(10 10 20 30 20 20 X Y Y) 
                           3)
  
  ;; Тест 2: Nil та числа. Поріг N=2. Всі пари (і nil, і 5) стискаються до одного.
  ;; Перевіряє коректну обробку nil як елемента даних.
  (check-delete-duplicates "test 2" 
                           '(nil nil 5 5 nil 8 nil nil nil) 
                           '(nil 5 nil 8 nil) 
                           2)
  
  ;; Тест 3: Змішаний набір. Група в кінці.
  ;; 8-ки (3 шт) >= 3 -> стискаються. 4-ки (2 шт) < 3 -> залишаються.
  (check-delete-duplicates "test 3" 
                           '(7 5 8 8 8 0 0 4 4 5 5 5) 
                           '(7 5 8 0 0 4 4 5) 
                           3)
                           
  (format t "End delete-duplicates tests~%"))

(test-spread-values)
(test-delete-duplicates)