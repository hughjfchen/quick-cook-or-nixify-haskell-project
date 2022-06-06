
const postgrestAuth = {
  login: ({ username, password }) => {
    const request = new Request('http://www.detachmentsoft.top/rest/rpc/login', {
      method: 'POST',
      body: JSON.stringify({ email: username, password }),
      headers: new Headers({ 'Content-Type': 'application/json'
                             , 'Accept': 'application/vnd.pgrst.object+json'
                             , 'Prefer': 'return=representation'
                           }),
    });
    return fetch(request)
      .then(response => {
        if (response.status < 200 || response.status >= 300) {
          throw new Error(response.statusText);
        }
        return response.json();
      })
      .then(({ me, token }) => {
        localStorage.setItem('token', token);
        localStorage.setItem('me', JSON.stringify(me));
      });
  },
  checkError: (error) => {
    const status = error.status;
    if (status === 401 || status === 403) {
      localStorage.removeItem('token');
      localStorage.removeItem('me');
      return Promise.reject();
    }
    return Promise.resolve();
  },
  checkAuth: () => {
    return localStorage.getItem('token') ? Promise.resolve() : Promise.reject();
  },
  logout: () => {
    localStorage.removeItem('token');
    localStorage.removeItem('me');
    return Promise.resolve();
  },
  getIdentity: () => {
    try {
      const { id, name, email, role } = JSON.parse(localStorage.getItem('me'));
      return Promise.resolve({ id, fullName:name });
    } catch (error) {
      return Promise.reject(error);
    }
  },
  getPermissions: () => {
    const role = JSON.parse(localStorage.getItem('me')).role;
    return role ? Promise.resolve(role) : Promise.reject();
  }
};

export default postgrestAuth;
