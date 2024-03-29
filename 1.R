library("lmtest")

data = swiss
#���������� ����� ������ swiss
#����������:
#   �����������: Infant.Mortality 
#   �����������: Agriculture | Examination
#�������:
#1) ������� ������� ��������, ��������� � ��� ����������
mean(data$Infant.Mortality)
# 19.94255
mean(data$Agriculture)
# 50.65957 
mean(data$Examination)
# 16.48936 ������� ����� ������������ ���������� ���������� ������, ��� ���������� ������� ����� �� ��������

var(data$Infant.Mortality)
# 8.483802 ��������� ����, ���������� �� �������� �� ������
var(data$Agriculture)
# 515.7994 ������� (��������� 100) ���������, ���������� �� �������� ����� ���� ������
var(data$Examination)
# 63.64662 ������� (������ 100, ������ 10) ���������...

sd(data$Infant.Mortality)
# 2.912697 ��� �����...
sd(data$Agriculture)
# 22.71122 ��� �������...
sd(data$Examination)
# 7.977883 ��� �� �������...

#2) ��������� ����������� ���� y = a + bx 3) � 4) ������ �������, ������ ����������� ����������� ���������� �� �����������

model1 = lm(Infant.Mortality~Agriculture,data)
summary(model1)
#Infant.Mortality = 20.338 - 0.008 * Agriculure
# ����������� �������������
#Pr(agriculture) = 0.684 (>0.005)
#��������-������������ ����� ���
#Pr(Coef) <2e-16
#���� ����������� �� ����-��

#p-value: 0.6845 > 0.05 (������� �������)
#Multiple R-squared:  0.003704,	Adjusted R-squared:  -0.01844
#����������� ���. ������ �� ���������� ����������������

model2 = lm(Infant.Mortality~Examination,data)
summary(model2)
#Infant.Mortality = 20.62899 -0.04163 * Examination
# ����������� �������������
#Pr(Examination) = 0.445 (>0.005) 
#��������-������������ ����� ���
#Pr(Coef) <2e-16
#���� ����������� �� ����-��

#p-value: 0.4454 > 0.05 (������� �������)
#Multiple R-squared:  0.013,	Adjusted R-squared:  -0.008932
#����������� ���. ������ �� ���������� ����������������

#�����:
#� Infant.Mortality ��� �������� ������������ �� Examination � Agriculture
#������� ���������� ���������� �� ������� �� ����, ��� ���� ����� ��������, � �� ����, �������� ����� ��� ��� ���
#�������� ����� ����� ���� ��, ��� ������� �������� �� ��������� ��� ����� ���������� �������� �������� � � �����

#����������:
#�� �� �������� �������, ��������� ������������� �������� Infant.Mortality
#���������� ������ (model1 | model2) �� ���������� ����������������

