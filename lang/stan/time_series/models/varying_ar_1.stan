data {
  int N_Medicine;          // 薬を投与した日数
  int N;                   // 合計日数
  vector[N] bloodPressure; // 収縮期血圧値
  vector[N] medicine;      // 投与した薬の量
}

parameters {
  real muZero;                          // 血圧の「状態」の初期値
  vector[N] mu;                         // 血圧の「状態」
  real<lower=0> sigmaV;                 // 観測誤差の大きさ
  real<lower=0> sigmaW;                 // 過程誤差の大きさ
  real coefMedicineTrendZero;           // 薬の効果のトレンドの初期値
  real coefMedicineZero;                // 薬の効果の初期値
  vector[N_Medicine] coefMedicineTrend; // 薬の効果のトレンド
  vector[N_Medicine] coefMedicine;      // 薬の効果
  vector[N_Medicine] coefMedicineReal;  // ノイズの入った後の薬の効果
  real<lower=0> sigmaWcoef;             // 薬の係数の過程誤差の大きさ
  real<lower=0> sigmaWcoefTrend;        // 薬の係数のトレンドの過程誤差の大きさ
  real<lower=0> sigmaVcoef;             // 薬の係数の観測誤差の大きさ
}

model {
  /* 状態方程式の部分 */
  // 血圧の状態の推定
  // 左端から初年度の状態を推定する
  mu[1] ~ normal(muZero, sigmaW/100);  

  // 血圧の状態の遷移
  for(i in 2:N) {
    mu[i] ~ normal(mu[i-1], sigmaW/100);
  }

  // 薬の効果の変化をモデル化する部分
  // 初年度の薬の効果のトレンドを推定する
  coefMedicineTrend[1] ~ normal(coefMedicineTrendZero, sigmaWcoefTrend/100); 

  // 薬の効果のトレンドの遷移
  for(i in 2:N_Medicine) {
    coefMedicineTrend[i] ~ normal(coefMedicineTrend[i-1], sigmaWcoefTrend/100);
  }

  // 初年度の薬の効果を推定する
  coefMedicine[1] ~ normal(coefMedicineTrend[1] + coefMedicineZero, sigmaWcoef/100);  

  // 薬の効果の遷移
  for(i in 2:N_Medicine) {
    coefMedicine[i] ~ normal(coefMedicineTrend[i] + coefMedicine[i-1], sigmaWcoef/100);
  }

  // 薬の効果にノイズが入る
  for(i in 1:N_Medicine) {
    coefMedicineReal[i] ~ normal(coefMedicine[i], sigmaVcoef/100);
  }

  /* 観測方程式の部分(薬なし) */
  for(i in 1:10) {
    bloodPressure[i] ~ normal(mu[i], sigmaV/100);
  }

  /* 観測方程式の部分(薬あり) */
  for(i in 11:N) {
    bloodPressure[i] ~ normal(mu[i] + coefMedicineReal[i-10]*medicine[i], sigmaV/100);
  }
}