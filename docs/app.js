(() => {
  'use strict';
  const citations = {
    crisis: '伊﨑直志（2026）「イギリス労働党における『危機』言説の展開（1975–1984年）」『同志社政策科学研究』28(1): 47–58.',
    galtan: '伊﨑直志（2025）「欧州政治におけるGAL–TAN対立軸研究の展開と展望―政党間競争の視角から」『同志社政策科学研究』27(1): 39–49. https://doi.org/10.14988/0002001265',
    democracy: 'プシェヴォスキ，アダム（2023）『民主主義の危機―比較分析が示す変容』吉田徹・伊﨑直志訳，白水社.'
  };
  const sessions = [
    ['イントロダクション：政治と政策','Introduction: politics and policy'],
    ['政策形成①：政策課題の形成','Policy formation I: agenda-setting'],
    ['政策形成②：政党と政策競争','Policy formation II: parties and policy competition'],
    ['政策決定①：政策決定の過程','Decision-making I: decision processes'],
    ['政策決定②：政策決定の制度','Decision-making II: institutions'],
    ['政策実施①：実施過程と現場の裁量','Implementation I: processes and discretion'],
    ['政策実施②：行政組織と政策フィードバック','Implementation II: organisations and policy feedback'],
    ['政策評価①：政策の効果を測る','Evaluation I: assessing policy effects'],
    ['政策評価②：評価とアカウンタビリティ','Evaluation II: accountability'],
    ['政策構造①：資源','Policy structures I: resources'],
    ['政策構造②：時間','Policy structures II: time'],
    ['政策構造③：空間','Policy structures III: space'],
    ['政策構造④：領域','Policy structures IV: territory'],
    ['総括：政治から政策へ、政策から政治へ','Conclusion: from politics to policy, and back']
  ];
  let lang = 'ja';
  const courseDialog = document.getElementById('course-dialog');
  const citationDialog = document.getElementById('citation-dialog');
  let toastTimeout;
  function updateSessions() {
    const list = document.querySelector('.session-list');
    list.replaceChildren(...sessions.map(session => {
      const li = document.createElement('li');
      li.textContent = session[lang === 'ja' ? 0 : 1];
      return li;
    }));
  }
  function setLanguage(next) {
    lang = next === 'en' ? 'en' : 'ja';
    document.documentElement.lang = lang;
    document.title = lang === 'ja' ? '伊﨑直志 | 比較政治学・政党政治' : 'Naoshi Isaki | Comparative Politics & Party Politics';
    document.querySelectorAll('[data-language]').forEach(button => button.setAttribute('aria-pressed', String(button.dataset.language === lang)));
    document.querySelectorAll('.dialog-close').forEach(button=>button.setAttribute('aria-label',lang === 'ja' ? '閉じる' : 'Close'));
    updateSessions();
    try { localStorage.setItem('isaki-site-language',lang); } catch (_) {}
  }
  document.querySelectorAll('[data-language]').forEach(button => button.addEventListener('click',()=>setLanguage(button.dataset.language)));
  let preferred = 'ja';
  try { preferred = localStorage.getItem('isaki-site-language') || 'ja'; } catch (_) {}
  setLanguage(preferred);
  document.querySelectorAll('[data-filter]').forEach(button => button.addEventListener('click',() => {
    const filter = button.dataset.filter;
    document.querySelectorAll('[data-filter]').forEach(other => {
      const active = other === button;
      other.classList.toggle('active',active);
      other.setAttribute('aria-pressed',String(active));
    });
    let count = 0;
    document.querySelectorAll('.publication').forEach(publication => {
      const show = filter === 'all' || publication.dataset.type === filter;
      publication.hidden = !show;
      if(show)count++;
    });
    document.getElementById('filter-status').textContent = lang === 'ja' ? `${count}件の業績を表示しています。` : `Showing ${count} ${count === 1 ? 'publication' : 'publications'}.`;
  }));
  function toast(message) {
    const el = document.getElementById('toast');
    clearTimeout(toastTimeout);
    el.textContent = message;
    el.classList.add('visible');
    toastTimeout = setTimeout(()=>el.classList.remove('visible'),2800);
  }
  document.querySelectorAll('.copy-citation').forEach(button=>button.addEventListener('click',async()=>{
    const citation = citations[button.dataset.citation];
    try {
      if(!navigator.clipboard)throw new Error('Clipboard unavailable');
      await navigator.clipboard.writeText(citation);
      toast(lang === 'ja' ? '引用情報をコピーしました' : 'Citation copied to clipboard');
    } catch (_) {
      document.getElementById('citation-text').value = citation;
      citationDialog.showModal();
      document.getElementById('citation-text').focus();
      document.getElementById('citation-text').select();
    }
  }));
  document.querySelector('.citation-close').addEventListener('click',()=>citationDialog.close());
  document.getElementById('open-course').addEventListener('click',()=>courseDialog.showModal());
  courseDialog.querySelectorAll('.dialog-close,.dialog-close-bottom').forEach(button=>button.addEventListener('click',()=>courseDialog.close()));
  [courseDialog,citationDialog].forEach(dialog=>dialog.addEventListener('click',event=>{
    const rect = dialog.getBoundingClientRect();
    if(event.target === dialog && (event.clientX < rect.left || event.clientX > rect.right || event.clientY < rect.top || event.clientY > rect.bottom))dialog.close();
  }));
})();
