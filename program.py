import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
import warnings
warnings.filterwarnings('ignore')

plt.rcParams['font.family'] = ['SimHei']
plt.rcParams['axes.unicode_minus'] = False


# 1. 数据读取和预处理
data = pd.read_excel("D:/aaa/sjfx/program/财务指标2022（全A股）.xlsx")

# 创建数据框
df = pd.DataFrame({
    'stock_code': data['证券代码'],
    'total_asset': data['总资产'] / 1e8,
    'current_ratio': data['流动比率'],
    'net_profit': data['净利润'] / 1e8,
    'ROA': data['资产收益率'],
    'ROE': data['净资产收益率'],
    'equity': data['股东权益合计'] / 1e8,
    'cfo': data['经营活动产生的现金流量净额'] / 1e8
})

# 设置索引
df.set_index('stock_code', inplace=True)

# 数据预处理
# 修正不合理负值
for col in ['current_ratio', 'ROA', 'ROE']:
    df[col] = df[col].apply(lambda x: 0 if x < 0 else x)

# 删除缺失值
df = df.dropna()

print("数据前10行:")
print(df.iloc[:10, :7])
print("\n" + "="*60)

# 2. 描述性统计分析
print("===== A股2022年8大核心财务指标 描述性统计 =====")
# 基础描述统计
base_summary = df.describe().T
base_summary = base_summary[['count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max']]
print("\n基本统计量:")
print(base_summary)

# 计算离散程度指标
base_desc = pd.DataFrame({
    '标准差': df.std().round(4),
    '四分位距': (df.quantile(0.75) - df.quantile(0.25)).round(4)
})
print("\n===== 离散程度指标 =====")
print(base_desc)

# 极值指标
base_extreme = pd.DataFrame({
    '最大值': df.max(),
    '最小值': df.min()
})
print("\n===== 极值指标 =====")
print(base_extreme)

print("\n" + "="*60)

# 3. 图表绘制
# 图表1：盈利三核心指标箱线图
fig1, axes1 = plt.subplots(1, 3, figsize=(15, 5), constrained_layout=True)
fig1.suptitle('A股2022年盈利核心指标分布', fontsize=16, fontweight='bold')

# 净利润
axes1[0].boxplot(df['net_profit'], patch_artist=True, 
                boxprops=dict(facecolor='#66c2a5', alpha=0.7),
                medianprops=dict(color='red'))
axes1[0].set_title('净利润(亿)')
axes1[0].set_ylabel('指标数值')

# ROA
axes1[1].boxplot(df['ROA'], patch_artist=True,
                boxprops=dict(facecolor='#fc8d62', alpha=0.7),
                medianprops=dict(color='red'))
axes1[1].set_title('ROA')
axes1[1].set_ylabel('指标数值')

# ROE
axes1[2].boxplot(df['ROE'], patch_artist=True,
                boxprops=dict(facecolor='#8da0cb', alpha=0.7),
                medianprops=dict(color='red'))
axes1[2].set_title('ROE')
axes1[2].set_ylabel('指标数值')

plt.show()

# 图表2：规模+偿债指标对数刻度箱线图
fig2, axes2 = plt.subplots(1, 2, figsize=(12, 5), constrained_layout=True)
fig2.suptitle('A股2022年企业规模与短期偿债能力分布', fontsize=16, fontweight='bold')

# 总资产（对数刻度）
axes2[0].boxplot(df['total_asset'], patch_artist=True,
                boxprops=dict(facecolor='#a6d854', alpha=0.7),
                medianprops=dict(color='red'))
axes2[0].set_yscale('log')
axes2[0].set_title('总资产(亿)')
axes2[0].set_ylabel('指标数值(对数刻度)')

# 流动比率
axes2[1].boxplot(df['current_ratio'], patch_artist=True,
                boxprops=dict(facecolor='#ffd92f', alpha=0.7),
                medianprops=dict(color='red'))
axes2[1].set_title('流动比率')
axes2[1].set_ylabel('指标数值')

plt.show()

# 图表3：经营现金流净额密度分布图
fig3, ax3 = plt.subplots(figsize=(10, 6))
sns.kdeplot(df['cfo'], fill=True, color='steelblue', alpha=0.5, ax=ax3)
ax3.axvline(df['cfo'].median(), color='red', linestyle='--', linewidth=2, label=f'中位数: {df["cfo"].median():.2f}亿')
ax3.axvline(df['cfo'].mean(), color='blue', linestyle='--', linewidth=2, label=f'平均数: {df["cfo"].mean():.2f}亿')
ax3.set_xlim(-50, 250)
ax3.set_title('A股2022年经营现金流净额分布', fontsize=14, fontweight='bold')
ax3.set_xlabel('经营活动现金流净额(亿)')
ax3.set_ylabel('密度')
ax3.legend()
plt.show()

# 图表4：ROA分位数直方图+密度曲线
fig4, ax4 = plt.subplots(figsize=(10, 6))
n, bins, patches = ax4.hist(df['ROA'], bins=30, density=True, alpha=0.6, color='lightgreen', edgecolor='black')
sns.kdeplot(df['ROA'], color='darkgreen', linewidth=2, ax=ax4)

# 添加分位数线
quantiles = df['ROA'].quantile([0.25, 0.5, 0.75])
colors = ['red', 'red', 'red']
labels = ['25%', '50%', '75%']
for q, color, label in zip(quantiles, colors, labels):
    ax4.axvline(q, color=color, linestyle='--', linewidth=1.5)
    ax4.text(q-0.01, 8, label, color='red', fontsize=10)

ax4.set_title('A股2022年ROA分布（直方图+密度曲线）', fontsize=14, fontweight='bold')
ax4.set_xlabel('资产收益率(ROA)')
ax4.set_ylabel('密度/频数')
plt.show()

# 图表5：ROE分组核密度分布图
# 创建ROE分组
df['ROE_level'] = pd.cut(df['ROE'], 
                        bins=[-np.inf, 0.05, 0.1, np.inf],
                        labels=['低回报(≤5%)', '中回报(5%-10%)', '高回报(≥10%)'])

fig5, ax5 = plt.subplots(figsize=(10, 6))
colors = ['#1b9e77', '#d95f02', '#7570b3']
for level, color in zip(df['ROE_level'].cat.categories, colors):
    subset = df[df['ROE_level'] == level]
    sns.kdeplot(subset['ROE'], label=level, color=color, fill=True, alpha=0.3, ax=ax5)

ax5.set_title('A股2022年ROE股东回报梯队分布', fontsize=14, fontweight='bold')
ax5.set_xlabel('净资产收益率(ROE)')
ax5.set_ylabel('密度')
ax5.legend(title='股东回报梯队')
plt.show()

# 图表6：流动比率风险等级环形图
df['debt_risk'] = pd.cut(df['current_ratio'],
                        bins=[-np.inf, 1, 2, np.inf],
                        labels=['高风险(＜1)', '合理区间(1-2)', '低风险(≥2)'])

debt_count = df['debt_risk'].value_counts().sort_index()
debt_ratio = debt_count / debt_count.sum() * 100

fig6, ax6 = plt.subplots(figsize=(8, 8))
colors = ['#619CFF', '#F8766D', 'lightgreen']
wedges, texts, autotexts = ax6.pie(debt_ratio.values, labels=debt_ratio.index,
                                   autopct='%1.1f%%', startangle=90,
                                   colors=colors, wedgeprops=dict(width=0.3))

# 美化标签
for autotext in autotexts:
    autotext.set_color('white')
    autotext.set_fontweight('bold')
    autotext.set_fontsize(10)

centre_circle = plt.Circle((0, 0), 0.70, fc='white')
ax6.add_artist(centre_circle)
ax6.set_title('A股2022年短期偿债风险等级分布', fontsize=14, fontweight='bold', y=1.05)
plt.show()

# 图表7：相关性热力图
corr_cols = ['total_asset', 'current_ratio', 'net_profit', 'ROA', 'ROE', 'equity', 'cfo']
corr_matrix = df[corr_cols].corr()

# 创建掩码矩阵（只显示左下三角）
mask = np.triu(np.ones_like(corr_matrix, dtype=bool))

fig7, ax7 = plt.subplots(figsize=(10, 8))
sns.heatmap(corr_matrix, mask=mask, annot=True, fmt='.2f', cmap='RdBu_r',
           center=0, square=True, linewidths=1, cbar_kws={"shrink": 0.8},
           ax=ax7)

# 设置标签
tick_labels = ['总资产', '流动比率', '净利润', 'ROA', 'ROE', '股东权益', '经营现金流']
ax7.set_xticklabels(tick_labels, rotation=45, ha='right')
ax7.set_yticklabels(tick_labels, rotation=0)
ax7.set_title('A股2022年核心财务指标相关性热力图', fontsize=16, fontweight='bold', pad=20)
plt.tight_layout()
plt.show()

# 图表8：ROE与ROA散点图+回归线
fig8, ax8 = plt.subplots(figsize=(10, 6))
sns.regplot(x='ROA', y='ROE', data=df, scatter_kws={'alpha':0.5, 's':20, 'color':'navy'},
           line_kws={'color':'red', 'linewidth':2}, ci=95, ax=ax8)

# 添加回归方程
from scipy import stats
slope, intercept, r_value, p_value, std_err = stats.linregress(df['ROA'], df['ROE'])
eq_text = f'拟合公式: ROE = {intercept:.4f} + {slope:.4f} * ROA\n^2$ = {r_value**2:.4f}'
ax8.text(0.2, 0.02, eq_text, transform=ax8.transAxes, fontsize=10,
         bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

ax8.set_title('A股2022年ROE与ROA关联关系', fontsize=14, fontweight='bold')
ax8.set_xlabel('资产收益率(ROA)')
ax8.set_ylabel('净资产收益率(ROE)')
plt.show()

# 图表9：净利润与经营现金流散点图
# 创建盈利质量分组
conditions = [
    (df['net_profit'] > 0) & (df['cfo'] > 0),
    (df['net_profit'] > 0) & (df['cfo'] < 0),
    (df['net_profit'] < 0) & (df['cfo'] > 0),
    (df['net_profit'] < 0) & (df['cfo'] < 0)
]
choices = ['优质(利润+现金流双正)', '存疑(利润正，现金流负)', 
           '潜力(利润负，现金流正)', '劣质(利润+现金流双负)']
df['profit_quality'] = np.select(conditions, choices, default='未知')

fig9, ax9 = plt.subplots(figsize=(12, 8))
colors = {'优质(利润+现金流双正)': 'forestgreen',
          '存疑(利润正，现金流负)': 'darkorange',
          '潜力(利润负，现金流正)': 'royalblue',
          '劣质(利润+现金流双负)': 'firebrick'}

for quality, color in colors.items():
    subset = df[df['profit_quality'] == quality]
    ax9.scatter(subset['net_profit'], subset['cfo'], 
               alpha=0.6, s=30, label=quality, color=color, edgecolor='white', linewidth=0.5)

# 添加坐标轴
ax9.axhline(y=0, color='black', linestyle='--', linewidth=1.5)
ax9.axvline(x=0, color='black', linestyle='--', linewidth=1.5)

ax9.set_xlim(-500, 1000)
ax9.set_ylim(-200, 1000)
ax9.set_title('A股2022年净利润与经营现金流关联分析（盈利质量四象限）', 
             fontsize=14, fontweight='bold')
ax9.set_xlabel('净利润(亿)')
ax9.set_ylabel('经营现金流净额(亿)')
ax9.legend(title='盈利质量等级')
ax9.grid(True, linestyle=':', alpha=0.5)
plt.show()

# 图表10：PCA降维聚类散点图
# 准备数据
seed = 40
np.random.seed(seed)

# 缩尾处理函数
def winsorize(series, min_val=0.005, max_val=0.995):
    lower = series.quantile(min_val)
    upper = series.quantile(max_val)
    return series.clip(lower, upper)

# 选择财务指标并进行缩尾处理
finance_cols = ['total_asset', 'current_ratio', 'net_profit', 'ROA', 'ROE', 'equity', 'cfo']
df_finance = df[finance_cols].copy()

for col in finance_cols:
    df_finance[col] = winsorize(df_finance[col])

# 标准化
scaler = StandardScaler()
df_scaled = scaler.fit_transform(df_finance)

# K-Means聚类
kmeans = KMeans(n_clusters=4, random_state=seed, n_init=20)
clusters = kmeans.fit_predict(df_scaled)
df_finance['财务类型'] = clusters + 1  # 从1开始编号

# PCA降维
pca = PCA(n_components=2, random_state=seed)
pca_result = pca.fit_transform(df_scaled)
df_finance['PC1'] = pca_result[:, 0]
df_finance['PC2'] = pca_result[:, 1]

# 财务类型标签
cluster_labels = {1: '盈利低效型', 2: '劣质风险型', 3: '规模优质型', 4: '成长潜力型'}
df_finance['财务标签'] = df_finance['财务类型'].map(cluster_labels)

# 计算聚类中心
cluster_center = df_finance.groupby('财务类型')[['PC1', 'PC2']].mean().reset_index()
cluster_center['财务标签'] = cluster_center['财务类型'].map(cluster_labels)

fig10, ax10 = plt.subplots(figsize=(12, 10))
colors = {1: '#e41a1c', 2: '#377eb8', 3: '#4daf4a', 4: '#984ea3'}

# 绘制散点
for cluster in range(1, 5):
    subset = df_finance[df_finance['财务类型'] == cluster]
    ax10.scatter(-subset['PC1'], -subset['PC2'], 
                alpha=0.7, s=50, 
                color=colors[cluster], 
                label=cluster_labels[cluster],
                edgecolors='white', linewidths=1)

# 绘制聚类中心
for i, row in cluster_center.iterrows():
    ax10.scatter(-row['PC1'], -row['PC2'], 
                s=300, color=colors[row['财务类型']], 
                marker='X', edgecolors='black', linewidths=2)
    ax10.text(-row['PC1']+0.1, -row['PC2'], 
             f'{int(row["财务类型"])}', fontsize=12, fontweight='bold')
    ax10.text(-row['PC1']+0.5, -row['PC2'], 
             row['财务标签'], fontsize=10, fontweight='bold')

# 添加坐标轴
ax10.axhline(y=0, color='gray', linestyle=':', linewidth=2, alpha=0.7)
ax10.axvline(x=0, color='gray', linestyle=':', linewidth=2, alpha=0.7)

ax10.set_xlabel('企业综合体量因子 (PC1)', fontsize=12, fontweight='bold')
ax10.set_ylabel('盈利能力质量因子 (PC2)', fontsize=12, fontweight='bold')
ax10.set_title('A股上市公司财务分层聚类可视化', fontsize=16, fontweight='bold', pad=20)
ax10.legend(title='财务类型', bbox_to_anchor=(1.05, 1), loc='upper left')
ax10.grid(True, linestyle=':', alpha=0.3)

# 添加主成分解释度
explained_var = pca.explained_variance_ratio_
ax10.text(0.02, 0.98, f'PC1解释方差: {explained_var[0]:.1%}\nPC2解释方差: {explained_var[1]:.1%}',
         transform=ax10.transAxes, fontsize=10,
         bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

plt.tight_layout()
plt.show()

# 4. 输出聚类统计信息
print("\n" + "="*60)
print("财务分层聚类统计信息:")
cluster_stats = df_finance.groupby('财务标签').agg({
    'total_asset': ['mean', 'std'],
    'ROA': ['mean', 'std'],
    'ROE': ['mean', 'std'],
    'current_ratio': ['mean', 'std']
}).round(4)
print(cluster_stats)