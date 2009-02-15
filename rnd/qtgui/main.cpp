#include <QApplication>
#include <QSplitter>
#include <QTreeView>
#include <QImage>
#include <QPainter>

#define WINDOW_SIZE 256
unsigned char *img;

class MyWidget : public QWidget
{
public:
    MyWidget(QWidget *parent = 0);

protected:
    void paintEvent(QPaintEvent *event);

private:
    QImage *image;
};

MyWidget::MyWidget(QWidget *parent) : QWidget(parent)
{
    setFixedSize(WINDOW_SIZE + 30, WINDOW_SIZE + 30);

    image = new QImage(img, WINDOW_SIZE, WINDOW_SIZE, QImage::Format_RGB32);
}

void
MyWidget::paintEvent(QPaintEvent *event)
{
    QPainter painter(this);

    printf("paint\n");

    painter.drawImage(15, 15, *image);
}


void
init()
{
    int i, j;

    img = new unsigned char[WINDOW_SIZE * WINDOW_SIZE * 4];

    for (j = 0; j < WINDOW_SIZE; j++) {
        for (i = 0; i < WINDOW_SIZE; i++) {
            img[4 * (j * WINDOW_SIZE + i) + 0] = 255;
            img[4 * (j * WINDOW_SIZE + i) + 1] = i;
            img[4 * (j * WINDOW_SIZE + i) + 2] = j;
            img[4 * (j * WINDOW_SIZE + i) + 3] = 0;
        }
    }
    
}

void
draw()
{


}


int
main(int argc, char **argv)
{
    init();

    QApplication app(argc, argv);
    MyWidget widget;
    //QImage *image = new QImage(img, WINDOW_SIZE, WINDOW_SIZE, QImage::Format_RGB32);

    //image->show();
    widget.show();

    return app.exec();
}
