library("lmtest")

#����� attitude
data = attitude
#help(attitude)

#����������:
#   �����������
#      rating
#   ��������� 
#       complaints | learning | raises

mean(data$rating) #64.63333
mean(data$complaints)#66.6
mean(data$learning)#56.36667
mean(data$raises)#64.63333

var(data$rating)#148.1713
var(data$complaints)#177.2828
var(data$learning)#137.7575
var(data$raises)#108.1023

sd(data$rating)#12.17256
sd(data$complaints)#13.31476
sd(data$learning)#11.73701
sd(data$raises)#10.39723

#������� �������� ������ ���� � �����
#������� �������� �������

#������:
#1) ���������, ��� � ������ ������ ��� �������� �����������
  #1. ������
    model_test_1 = lm(complaints~learning,data)
    summary(model_test_1)
    #y = 28.442 + 0.677 * x
    # ����������� �������������
    #pr = 0.00050
    #R^2 = 0.3561
    #p-value: 5e-04 < 0.05
    #����������� ����� ������������ ������������
    #
    
    model_test_2 = lm(complaints~raises,data)
    summary(model_test_2)
    #y = 11.21 + 0.857*x
    # ����������� �������������
    #pr = 5.27e-05
    #R^2 = 0.4478
    #p-value: 5.268e-05 < 0.05
    # ����������� ����� ������������ ������������
    #
    
    model_test_3 = lm(raises~learning,data)
    summary(model_test_3)
    #y = 32.6609 + 0.5672 * x
    # ����������� �������������
    #pr = 0.000138
    #R^2 = 0.41
    #p-value: 0.0001384 < 0.05
    #����������� ����� ������������ ������������
    #
    
  #2. ������
    model1 = lm(rating ~ complaints + learning + raises, data)                 #!
    summary(model1)
    vif(model1)
    #rating = 10.523 + 0.653 * complaints + 0.221 * learning - 0.029 * raises
    #pr (complaints) = 5.82e-05         rating ������� �� ������ ����������
    #pr (learning) = 0.152              rating �� ������� �� ������ ����������
    #pr (raises) = 0.876                rating �� ������� �� ������ ����������
    #p-value: 3.957e-07 < 0.05 | R^2 = 0.7083
    #vif ~~ 2 ��� ������� ����������� (���� �����������)
    #
  #����������� ����� ������������ ����
  #
#���������� ������� ���������� ���� ����� �����

#2) ��������� �������� ������
#mode1 ��������, ��� rating ����� ����� ���������� ����� complaints
#��������� �������� ������� ���������� �/��� ������ ������� ������������
#
    
#��� ������� ������, ������������ ����������� rating �� ����������� (model1), �������� ���� �� ����� ������ ������
    
model2 = lm(rating ~ learning + complaints +  raises, data)
summary(model2)
#��������� ����������
#rating ����� ����� ���������� ����� complaints
#������� ������������������� ����������

model3 = lm(rating ~ complaints, data)
summary(model3)
#R^2 = 0.6813
#p-value: 1.988e-08 < 0.05
#������ ����� ����� ������ (���� �� R^2)
#

model4 = lm(rating ~ learning + complaints, data)                 #!
summary(model4)
# rating  = 9.8709 + 0.2112 * learning + 0.6435 * complaints
#����������� ������������� ��� ����� �����������
#pr(learning) = 0.128 > 0.05
#pr(complaints) = 9.57e-06 < 0.05
#R^2 = 0.708
#p-value: 6.058e-08
#������ ����� ����� �� R^2, �� p-value ����������

model5 = lm(rating ~ complaints +  raises, data)
summary(model5)
#R^2 = 0.6839
#p-value: 1.769e-07
#������ ���� (���� �� R^2)
#������ ������� �� ���������� �� ������ 3, � ������� �������� �������� raises

modeljoke = lm(rating ~ learning +  raises, data)
summary(modeljoke)
#R^2 = 0.4507
#pr(learning) = 0.0333 < 0.05
#pr(raises) = 0.0930 > 0.05
#��� ����������� �� ������ ���������� (��� �������, ��� ��� � ��� ���� ����� � complaints)

#3) ������ � ������ ��������� �����������

#�������� raise, ��� ��� � ���� ���������� ������ ����������� �� ��������
model_1 = lm(rating ~ I(log(complaints))  + learning, data)
summary(model_1)
#R^2 = 0.7047 > 0.7
#p-value: 7.06e-08 < 0.05
#pr(I) = 1.12e-05  < 0.05
#����������� ��������� rating
#������ ���������

model_2 = lm(rating ~ I(log(learning)) + complaints, data)
summary(model_2)
#R^2 = 0.7018 > 0.7
#p-value:8.034e-08 < 0.05
#pr(I) = 0.184 > 0.05
#�����������  ��������� rating
#������ ���������, �� log �� ������� ���������

model_3 = lm(rating ~ I(log(complaints + learning)), data)
summary(model_3)
#R^2 = 0.6496 < 0.7
#p-value:7.204 7.68e-08 < 0.05
#pr(I) = 7.676e-08 < 0.05
#����������� ���� ��������� rating
#������ ���������, ������� ���� ����������

model_4 = lm(rating ~ I(log(complaints * learning)), data)
summary(model_4)
#R^2 = 0.6439 < 0.7
#p-value:9.636e-08 < 0.05
#pr(I) = 9.64e-08 < 0.05
#����������� ���� ��������� rating
#������ ���������, �� ����� ��, ��� � _3

model_5 = lm(rating ~ I(log(complaints)) + complaints + learning, data)
summary(model_5)
#R^2 = 0.7088> 0.7
#p-value: 3.866e-07 < 0.05
#pr(I) = 0.791 < 0.05
#����������� ��������� ��������� rating
#� ������ ��� ������ ������ ����������� �� ����������...
#������ "������"

#�����: log �� ���������� �� ��������� ������ � ������ ������
#

#4) ������� � ������ ������������ ������������ ��� �����������.������� ���� ��� ��������� ��������� ������� �� ���� ������������ �������� � ������ R^2


model__ = lm(rating ~ I() + complaints + learning, data)
summary(model__)
#R^2 = > 0.7
#p-value: < 0.05
#pr(I) =  < 0.05
#�����������  ��������� rating
#������ ���������

model__1 = lm(rating ~ I(complaints * learning), data)
summary(model__1)
#R^2 = 0.6626 < 0.7
#p-value: 4.476e-08< 0.05
#pr(I) = 4.48e-08 < 0.05
#����������� ���� ��������� rating

model__2 = lm(rating ~ I(learning * raises) + complaints , data)
summary(model__2)
#R^2 = 0.7022 > 0.7
#p-value: 7.896e-08 < 0.05
#pr(I) = 0.1798 > 0.05
#�������� I �� ������ �� ���������


model__3 = lm(rating ~ I(complaints*raises*learning), data)
summary(model__3)
#R^2 = 0.6057 < 0.7
#p-value:4.135e-07 < 0.05
#pr(I) = 4.13e-07 < 0.05
#����������� ����� ��������� rating
#������ ������ � complaints

#������ ����������� rating �� complaints, raises � learning, ����� ������ � ������
#rating ������ ����� ������� �� complaints � �� ������� ��  raises
#��������������� ������� � ������������ ������������ �� �������� ���������� ����������
#
#������ ������ - model1 � model4
#R^2 > 0.7


#������ ������ ������������� �������� ��� model4 ��� p = 95%
#p = 95%; p1 = p + (100 - p)/2 | p1 = 97.5%
#p2 = 0.975
#����� ������� 30 | ����� ����������� 2 => 30 - 2 = 28
#������ t-�������� ���������
t = qt(0.975,28)
#t = 2.048 �� ������, �� �� ������ ���������

#����������� ������ learning_q = 0.134
#����������� ������ complaints_q = 0.119


#������������� �������� ��� learning = [0.2112 - 0.274;0.2112 + 0.274] = [-0.0628;0.4852]
#0 ����������� ���������, ��� ��� �� �� ����� ���������� �������������� �������� � ���, ��� ����������� ����� 0

#������������� �������� ��� complaints = [0.6435 - 0.119 * 2.048; 0.6435 + 0.119 * 2.048] = [0.399788;0.887212]
#0 �� ����������� ���������, ��� ��� �� ����� ���������� �������������� �������� � ���, ��� ����������� ����� 0

#������ ��������� ������������� �������� ��� ��������
#������� complaints = 60, learning = 55
new.data = data.frame(complaints = 60, learning = 55)
predict(model4,new.data,interval = "confidence")
#������� ������ 60.09749
#������ ������� 57.17933
#������� ������� 63.01565

