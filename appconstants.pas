unit AppConstants;

{$mode ObjFPC}{$H+}

interface

const
  // GitHub-проект
  REPO_OWNER = 'warlocknt';
  REPO_NAME  = 'multiPingLed';

  // Полные URL
  URL_REPO            = 'https://github.com/' + REPO_OWNER + '/' + REPO_NAME;
  URL_RELEASES_LATEST = URL_REPO + '/releases/latest';

  // Путь для GitHub API (используется в TVersionCheckThread)
  API_PATH_LATEST_RELEASE = '/repos/' + REPO_OWNER + '/' + REPO_NAME + '/releases/latest';


implementation

end.

