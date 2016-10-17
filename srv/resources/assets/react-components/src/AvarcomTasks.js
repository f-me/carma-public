import React, { Component } from 'react';

import Typeahead from 'react-bootstrap-typeahead';
import 'react-bootstrap-typeahead/css/Typeahead.css';

const propTypes = {
};

const defaultProps = {
};


export default class AvarcomTasks extends Component {
  constructor(props) {
    super(props);
    this.state = {
      availableTasks: [],
      selectedTasks: []
    };

    $.ajax({
      type: 'GET',
      url: '/_/AvarcomTask',
      dataType: 'json',
      success: (res) =>
        this.setState({availableTasks: res.filter((x) => x.isActive)})
    });
  }

  _addTask = (tasks) => {
    const selectedTasks = this.state.selectedTasks.concat(tasks);
    const availableTasks = this.state.availableTasks.filter(
        (x) => !(x in selectedTasks));
    this.setState({
      selectedTasks: selectedTasks,
      availableTasks: availableTasks
    });
  }

  _removeTask = (task) => {
    return () => {};
  }

  render() {
    const selectedTasks = this.state.selectedTasks.map((task) => {
      return (
        <li key={task.id}>
          <input type="checkbox"/> &nbsp;
          {task.label}
          <a href="" onClick={this._removeTask(task)}>&nbsp; ×</a>
        </li>);
    });

    return (
      <div>
        <Typeahead emptyLabel="Ничего не найдено"
          options={this.state.availableTasks}
          onChange={this._addTask}
        />
        <ul>{selectedTasks}</ul>
      </div>);
  }
}

AvarcomTasks.propTypes = propTypes;
AvarcomTasks.defaultProps = defaultProps;
