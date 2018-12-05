# Desafio Dito
Desafio para concorrencia de vaga de cientista de dados na Dito, divido em duas partes, sendo a primeira delas um problema que envolve SQL e a segunda Análise Exploratória de Dados.

###### Assim começou minha análise:

![alt text](https://github.com/eleuuterio/desafiodito/blob/master/pictures/Capturar.PNG)

Eu quis saber qual era a cara dos dados que estava analisando, então gerei umas visualizações bem rápidas  usando Power BI, utilizando algumas medidas simples de quantidade, quantificando número de email Enviados, Abertos, Clicados, Classificados com Spam e que geraram “Unsubscribe”, atreladas a uma dimensão de tempo completa tive uma vista geral do cenário é pude começar a validar minhas hipóteses.

Quando enviamos um e-mail marketing queremos chamar a atenção e ser notados, uma excelente forma de se conseguir isso é ser o primeiro da lista de e-mail recebidos do usuário quando o usuário vai checar seus e-mail, pois essa posição de destaque chama mais a atenção.
As pessoas abrem um e-mail por vários motivos, entre eles destaco dois muito úteis para minha análise:

Para limpar as notificações, nem chegando a ler, pois estão esperando um email em especial;

-Na checagem rotineira de sua caixa de email, caso o e-mail não esteja em posição de destaque ou o assunto chame sua atenção, para limpar as notificações;

-Portanto, o fato de se abrir o e-mail não evidencia a melhor hora para se receber um email, ou seja, não mostra qual a melhor hora para enviar um  e-mail para esse usuário . 

Quando a pessoa da unsubscribe no e-mail ou clica no link do e-mail , ela realmente deu atenção para aquele e-mail, e resolveu tomar a decisão porque realmente estava checando o e-mail, é isso que queremos, a atenção dos usuários, por isso os melhores horários para enviar são os de cliques e unsubscribe, pois nesses momentos de tomada de decisão temos a certeza de ter conseguido a atenção do usuário, evidenciando que o email foi notado, e portanto estava em posição de destaque, ignorando-se seu conteúdo.

Primeiro, quis checar qual a relação entre o número de emails enviados, abertos e clicados. Se minhas hipóteses forem verdadeiras, um aumento no número de envios será seguido de um aumento no número de aberturas, uma vez que as aberturas de email são “automáticas”  pelos usuários.

![alt text](https://github.com/eleuuterio/desafiodito/blob/master/pictures/2.PNG)

Neste gráfico de Ribbon é possível ver que as variações entre enviados (vermelho no top) e abertos (azul claro na segunda posição) são similares, portanto as hipótese de abertura automática de emails com o simples objetivo de limpar os email pode ser aceita. 
Logo em seguida, quis entender como se relacionavam as aberturas de e-mails (azul claro no topo) e cliques (azul escuro na segunda posição), esse gráfico de Ribbon mostra que grandes aumentos e reduções nas taxas de abertura não causam grandes mudanças na taxa de cliques, isso fica bem claro nos meses Junho e Julho, mas também pode ser notado nos demais meses. Ou seja, mais um indício forte de que a abertura de e-mail não necessariamente evidência a melhor hora para envio de email.

![alt text](https://github.com/eleuuterio/desafiodito/blob/master/pictures/sem%20enviadas.PNG)

Nesse momento, resolvi assumir os cliques em emails como objeto de análise e passei a procurar o seu grande vilão.  Qual seria o oposto de um clique no email? Algo que requer conseguir a atenção do usuário e falhar em ser útil para ele, que o faz tirar seu tempo para analisar e perceber que aquele e-mail não o serve como objeto de informação. Pensei em dois candidatos óbvios, Reportar Spam e Unsubscribe.
Foi que fui ver como se comportam as duas categorias de maneira sumarizada ao longo do ano, e tentar entender qual das duas era mais antagônica ao clique. Para isso também utilizei um gráfico de Ribbon, com a distribuição de cliques, reportes de spam e unsubscribe:

![alt text](https://github.com/eleuuterio/desafiodito/blob/master/pictures/cliques%20spam%20e%20unsubriscrible.PNG)

O gráfico não apresenta nenhuma informação de fácil entendimento pois a quantidade de spams e unsubscribes é muito pequena se comparada a de cliques,  por questões de prazo reduzido optei por não fazer nenhum tipo de análise estatística avançada, como correlação, regras de associação, etc. Porém resolvi me apegar aos detalhes do processo e não somente aos dados e fiz uma pesquisa rápida com meus familiares e amigos, onde ficou fácil entender o que acontece nessa situação. O unsubscribe e a ação mais  similar (e antagônica) ao clique, uma vez que ambos requerem que você entre no e mail, passe por todo conteudo e por fim CLIQUE em unsubscribe, o usuário deve dar atenção ao email para se desinscrever da lista de recipientes, e para reportar spam ele apenas precisa arrastar o dedo sendo mais similar (e antagônico) ao ato de abrir o email para limpar a notificação.

Portanto, meu objeto de análise passou a ser as ações de clique e de unsubscribe, pois as duas retratam o momento em que conseguimos a atenção do usuário e portanto são os melhores momentos para se entrar em contato com ele.

A partir daí eu basicamente olhei como se comporta a distribuição dos cliques e unsubscribes ao longo dos quartos de ano, meses, dias e horas, e procurei um algoritmo de Send Time Optimization Time (STO) já implementado e disponíveis em literatura que seguisse essa mesma heurística. Fui feliz em minha busca por algoritmos em um blog de um cientista de dados Alemão especialista em Email Marketing, que possui um código implementado em R, fiz as adaptações necessárias no código fonte para aplicar aos dados em csv, mas infelizmente não obtive sucesso, e alguns erros apareceram. Por ser muito bem documentado e escrito em com uso das bibliotecas de Java e Weka em R, acredito que seja possível debugar, entretanto não me empenhei a tal atividade.

![alt text](https://github.com/eleuuterio/desafiodito/blob/master/pictures/cliques%20por%20hora.PNG)

A visão geral que tiro dessa análise e a de que, para otimizar o horário de envio de emails, precisamos descobrir em qual momento o usuário faz a conferência dos e-mails dando a eles a necessária atenção, e fazer com que nossos email cheguem nesse horário, ocupando posição de destaque em sua caixa de emails. 

Para isso, devemos buscar nos dados, informações que nos evidenciam horários em que o usuário dá grande atenção aos e-mail recebidos, como por exemplo ações de clique e unsubscribe. Mesmo que as ações de unsubscribe façam com que o usuario nao receba mais email de determinada lista, suas informações são ligadas aos seu email e podem ser utilizadas em listas de envios futuras.
